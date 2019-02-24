{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnicodeSyntax       #-}

import Prelude ( error, undefined )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import qualified  Data.Foldable  as  F

import Control.Applicative  ( empty, pure )
import Control.Exception    ( bracket )
import Control.Monad        ( fail, forM_, forM, join, mapM, mapM_, return )
import Data.Bool            ( Bool( True ), otherwise )
import Data.Bifunctor       ( bimap, first )
import Data.Char            ( Char, isAlphaNum )
import Data.Either          ( Either( Left, Right ), either, partitionEithers )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, concat )
import Data.Function        ( ($), (&), flip, id )
import Data.Functor         ( Functor, fmap )
import Data.List            ( (!!), intercalate, sortOn )
import Data.Maybe           ( Maybe( Just ), fromMaybe, maybe )
import Data.Monoid          ( mappend )
import Data.Ord             ( Ord, (<) )
import Data.String          ( IsString( fromString ), String )
import Data.Traversable     ( traverse )
import Data.Tuple           ( fst,snd )
import Data.Word            ( Word8 )
import GHC.Generics         ( Generic )
import System.Exit          ( ExitCode( ExitFailure ) )
import System.IO            ( FilePath, Handle, IO, hClose, putStrLn )
import Text.Read            ( read )
import Text.Show            ( Show, show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , parseText, toString, toText )

-- dhall -------------------------------

import qualified  Dhall       as  D
import qualified  Dhall.Core  as  DC

import Dhall  ( Interpret( autoWith ), Type( Type, expected, extract )
              , auto, field, record )

-- fluffy ------------------------------

import Fluffy.Applicative    ( (⊵), (∤), (⋪) )
import Fluffy.Functor2       ( (⊳) )
import Fluffy.Lens2          ( (⊣), (⊢) )
import Fluffy.IO.Error       ( AsIOError, IOError )
import Fluffy.IO.Error2      ( )
import Fluffy.Maybe          ( maybeE )
import Fluffy.Monad          ( (≫), (⪼) )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.MonadIO        ( MonadIO, liftIO, unlink )
import Fluffy.MonadIO.Error  ( eitherIOThrow )
import Fluffy.MonadIO2       ( die )
import Fluffy.Options        ( optParser )
import Fluffy.Parsec         ( Parsecable( parser ) )
import Fluffy.Path           ( AbsDir, AbsFile, AsPathError, RelFile
                             , getCwd_, parseFile' )
import Fluffy.Path2          ( )
import Fluffy.Printable      ( __ERR, q )
import Fluffy.TempFile2      ( pc, with2TempFiles' )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Getter  ( Getter, to )
import Control.Lens.Lens    ( Lens', lens )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- network-ip --------------------------

import Network.IP.Addr  ( IP4, ip4FromOctets )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, help )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, oneOf, string )
import Text.Parsec.Combinator  ( count, eof )
import Text.Parsec.Prim        ( ParsecT, Stream, parse, try )

-- path --------------------------------

import Path  ( File, Path, (</>), toFilePath )

-- proclib -----------------------------

import ProcLib.Error.ExecCreateError  ( ExecCreateError )
import ProcLib.Process                ( mkProc_, runProcIO )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.RunProcOpts      ( defRunProcOpts, dryRunL, verboseL )

-- streaming ---------------------------

import qualified  Streaming.Prelude  as  S

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text     ( Text
                     , all, null, pack, splitOn, takeWhile, unlines, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- Unique ------------------------------

import Data.List.UniqueUnsorted  ( repeated )

-- unordered-containers ----------------

import Data.HashMap.Strict  ( HashMap
                            , elems, foldrWithKey, fromList, lookup, toList )

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml
import Data.Yaml  ( FromJSON( parseJSON ), Value( Object, String )
                  , decodeEither' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  TinyDNS.Paths  as  Paths

--------------------------------------------------------------------------------

isAlphaNumU ∷ Char → Bool
isAlphaNumU '_' = True
isAlphaNumU c   = isAlphaNum c

------------------------------------------------------------

data Host = Host { fqdn ∷ FQDN
                 , ipv4 ∷ IPv4
                 , desc ∷ Text
                 }
  deriving (FromJSON, Generic, Show)

hostType ∷ Type Host
hostType = record $ Host ⊳ field "fqdn" (FQDN ⊳ D.strictText)
                         ⊵ field "ipv4" ip4Type
                         ⊵ field "desc" D.strictText

instance Interpret Host where
  autoWith _ = hostType

------------------------------------------------------------

newtype FQDN = FQDN Text
  deriving (Eq, Generic, Hashable, Ord, Show)

instance Printable FQDN where
  print (FQDN t) = P.text t

emptyPartFQDN ∷ Text → String
emptyPartFQDN t = "FQDN with empty part: '" ⊕ unpack t ⊕ "'"

noPartsFQDN ∷ Text → String
noPartsFQDN t = "FQDN with no parts: '" ⊕ unpack t ⊕ "'"

badFQDN ∷ Text → String
badFQDN t = "bad FQDN: '" ⊕ unpack t ⊕ "'"

instance FromJSON FQDN where
  parseJSON (String "")                = fail "empty FQDN"
  parseJSON (String t)
                   | F.length (splitOn "." t) < 2 = fail $ noPartsFQDN t
                   | F.any null (splitOn "." t) = fail $ emptyPartFQDN t
                   | F.all (all isAlphaNumU) (splitOn "." t) = return $ FQDN t
                   | otherwise         = fail $ badFQDN t
  parseJSON invalid                    = typeMismatch "short host name" invalid

localPart ∷ FQDN → UQDN
localPart (FQDN h) = UQDN (takeWhile (≢ '.') h)

------------------------------------------------------------

{- | hostname, no FQDN. -}
newtype UQDN = UQDN { unUQDN ∷ Text }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance Printable UQDN where
  print (UQDN h) = P.text h

badShortHostName ∷ Text → String
badShortHostName t = "bad short host name: '" ⊕ unpack t ⊕ "'"

uqdnType ∷ Type UQDN
uqdnType = UQDN ⊳ D.strictText

instance Interpret UQDN where
  autoWith _ = uqdnType

instance FromJSON UQDN where
  parseJSON (String "")                = fail "empty unqualified host name"
  parseJSON (String t)
                   | all isAlphaNumU t = return $ UQDN t
                   | otherwise         = fail $ badShortHostName t
  parseJSON invalid                    = typeMismatch "short host name" invalid

------------------------------------------------------------

type UQHMap = HashMap UQDN

------------------------------------------------------------

newtype IPv4 = IPv4 IP4
  deriving (Eq, Ord, Show)

instance FromJSON IPv4 where
  parseJSON (String t) = case parseText t of
                           Parsed ip4 → return $ IPv4 ip4
                           Malformed _ e → fail (e ⊕ " (" ⊕ unpack t ⊕ ")")
  parseJSON invalid    = typeMismatch "IPv4" invalid

instance Printable IPv4 where
  print (IPv4 ipv4) = P.text (toText ipv4)

------------------------------------------------------------

newtype HostMap = HostMap { unHostMap ∷ UQHMap Host }
  deriving Show

instance FromJSON HostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (UQDN, Host)
        go (k,Object v) = parseJSON (Object v) ≫ return ∘ (UQDN k,)
        go (k,invalid)  =
          typeMismatch (unpack $ "Host: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ (mapM go (toList hm)) ≫ \ case
          (hm' ∷ (HashMap UQDN Host)) → return $ HostMap hm'
  parseJSON invalid     = typeMismatch "host map" invalid

fromListE ∷ (Hashable κ, Eq κ, MonadError (RepeatedKeyError κ) η) ⇒
            [(κ,υ)] → η (HashMap κ υ)
fromListE kvs = case repeated $ fst ⊳ kvs of
                  []   → return $ fromList kvs
                  dups → throwError $ RepeatedKeyError dups

hmHosts ∷ HostMap → [Host]
hmHosts (HostMap hm) = elems hm

hostMapType ∷ Type HostMap
hostMapType = let localHNKey h = (localPart (fqdn h), h)
               in HostMap ∘ fromList ∘ fmap localHNKey ⊳ D.list hostType

instance Interpret HostMap where
  autoWith _ = hostMapType

------------------------------------------------------------

newtype ShortHostMap = ShortHostMap { unSHMap ∷ UQHMap UQDN }
  deriving Show

data LocalAlias = LocalAlias UQDN UQDN

localAliasType ∷ Type LocalAlias
localAliasType = record $ LocalAlias ⊳ field "from" uqdnType
                                     ⊵ field "to"   uqdnType

instance Interpret LocalAlias where
  autoWith _ = localAliasType

localAliasPair ∷ LocalAlias → (UQDN,UQDN)
localAliasPair (LocalAlias from to) = (from,to)

newtype RepeatedKeyError α = RepeatedKeyError [α]

instance Printable α ⇒ Printable (RepeatedKeyError α) where
  print (RepeatedKeyError ks) = P.text $ [fmt|repeated keys [%L]|] (q ⊳ ks)

shortHostMapType ∷ Type ShortHostMap
shortHostMapType =
  ShortHostMap ⊳ fromList ∘ fmap localAliasPair ⊳ D.list localAliasType

instance Interpret ShortHostMap where
  autoWith _ = shortHostMapType

instance FromJSON ShortHostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (UQDN, UQDN)
        go (k,String v) = return (UQDN k, UQDN v)
        go (k,invalid)  =
          typeMismatch (unpack $ "short host name: '" ⊕ k ⊕ "'") invalid
     in fmap ShortHostMap ∘ fmap fromList ∘ mapM go $ toList hm
  parseJSON invalid     = typeMismatch "short host map" invalid

------------------------------------------------------------

data Hosts = Hosts { hosts        ∷ HostMap
                   , dns_servers  ∷ [UQDN]
                   , mail_servers ∷ [UQDN]
                   , aliases      ∷ ShortHostMap
                   }
  deriving (FromJSON, Generic)


hostsType ∷ Type Hosts
hostsType = record $ Hosts ⊳ field "hosts"        hostMapType
                           ⊵ field "dns_servers"  (D.list uqdnType)
                           ⊵ field "mail_servers" (D.list uqdnType)
                           ⊵ field "aliases"      shortHostMapType

instance Interpret Hosts where
  autoWith _ = hostsType


instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:        " ⊕ show (hosts h)
                            , "DNS_SERVERS:  " ⊕ show (dns_servers h)
                            , "MAIL_SERVERS: " ⊕ show (mail_servers h)
                            , "ALIASES:      " ⊕ show (aliases h)
                            ]

lookupHost ∷ Hosts → UQDN → Maybe Host
lookupHost = flip lookup ∘ unHostMap ∘ hosts

hostIPv4 ∷ Hosts → UQDN → Maybe IPv4
hostIPv4 hs h = ipv4 ⊳ lookupHost hs h

hostIPv4' ∷ Hosts → UQDN → Either Text IPv4
hostIPv4' hs h = let quote t = "'" ⊕ toText t ⊕ "'"
                     noSuchH = "hostIPv4': no such host " ⊕ quote h
                 in maybe (Left noSuchH) Right $ hostIPv4 hs h

hostsHosts ∷ Hosts → [Host]
hostsHosts = hmHosts ∘ hosts

hostIPv4s ∷ Hosts → [(FQDN,IPv4)]
hostIPv4s = fmap ( \ h → (fqdn h, ipv4 h) ) ∘ hostsHosts

------------------------------------------------------------

data Options = Options { _input ∷ AbsFile }

input ∷ Lens' Options AbsFile
input = lens _input ( \ o i → o { _input = i } )

inputFilePath ∷ Getter Options FilePath
inputFilePath = input ∘ to (toFilePath)

------------------------------------------------------------

readAbsFile ∷ AbsDir → ReadM AbsFile
readAbsFile cwd = eitherReader go
                  where toLeft ∷ (β → α) → Either α β → α
                        toLeft = either id
                        eToAbs ∷ Either AbsFile RelFile → AbsFile
                        eToAbs = toLeft (cwd </>)
                        go ∷ String → Either String AbsFile
                        go = first show ∘ fmap eToAbs ⊳ parseFile' ∘ pack


parseOptions ∷ AbsDir → OptParse.Parser Options
parseOptions cwd = Options ⊳ argument (readAbsFile cwd) (help "HOSTS.YAML")

-- | Perform some IO within a temporary directory freshly created by `mkTempDir`.
--   Cleans away the created directory when IO is complete.

__loadFile__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFile__ f = liftIO $
  decodeEither' ⊳ readFile (toFilePath f) ≫ either (error ∘ show) return

domains ∷ [Text]
domains = ["sixears.co.uk", "0.168.192.in-addr.arpa"];

----------------------------------------

addNSCmd ∷ AbsFile → AbsFile → IPv4 → [CmdSpec]
addNSCmd fn tmpfn ip = ( \ d → CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "ns", toText d, toText ip ] ) ⊳ domains

----------------------------------------

addHostCmd ∷ AbsFile → AbsFile → Host → [CmdSpec]
addHostCmd fn tmpfn h = [ CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "host", toText $ fqdn h, toText $ ipv4 h ] ]

----------------------------------------

addAliasCmd ∷ AbsFile → AbsFile → FQDN → Host → CmdSpec
addAliasCmd fn tmpfn fqdn h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "alias", toText fqdn, toText $ ipv4 h ]

----------------------------------------

addMxCmd ∷ AbsFile → AbsFile → Host → CmdSpec
addMxCmd fn tmpfn h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "mx", toText (fqdn h), toText $ ipv4 h ]

----------------------------------------

main ∷ IO ()
main = do
  cwd  ← getCwd_
  opts ← optParser "make tiny dns data from hosts config" (parseOptions cwd)
  hs   ← __loadFile__ (opts ⊣ input)

  putStrLn (show hs)

  __withTemps__ $ __mkData__ hs

----------------------------------------

runProc ∷ CmdSpec → IO (Either ExecCreateError ())
runProc = splitMError ∘ runProcIO (defRunProcOpts & verboseL ⊢ 1) ∘ mkProc_

----------------------------------------

-- | Take a hashmap from α to Maybe β; and an error function; throw an error for any
--   `Nothing`s; otherwise return a hashmap from α to β.
-- allJusts ∷ MonadError ε η ⇒ (α → ε) → HashMap α (Maybe β) → η (HashMap α β)
-- allJusts err hm =

__mkData__ ∷ Hosts → (AbsFile, _z0) → (AbsFile, _z1) → IO ()
__mkData__ hs (fn1,_) (fn2,_) = do
  case partitionEithers $ ( \ h → hostIPv4' hs h) ⊳ dns_servers hs of
    ([], ips) → do forM_ ips $ \ ip → forM_ (addNSCmd fn1 fn2 ip) $ runProc

    (es, _)   → die (ExitFailure 3) (unlines es)

  forM_ (addHostCmd fn1 fn2 ⊳ (sortOn ipv4 $ hostsHosts hs)) (mapM_ runProc)

  case forM (unSHMap $ aliases hs) ( \ h → maybeE h (lookupHost hs h) ) of
    Left  h → die (ExitFailure 3) h
    Right as → mapM_ runProc $ foldrWithKey ( \ u h a → addAliasCmd fn1 fn2 (FQDN (toText u ⊕ "." ⊕ domains !! 0)) h : a) [] as

  case forM (( \ h → (h,lookupHost hs h)) ⊳ mail_servers hs) ( \ (hn,mh) → maybeE hn mh) of
    Left  h → die (ExitFailure 3) h
    Right as → mapM_ runProc $ ( addMxCmd fn1 fn2 ) ⊳ as


  TextIO.readFile (toString fn1) ≫ TextIO.putStrLn

  let dhall = "{ fqdn = \"fqdn\", desc = \"descn\", ipv4 = \"192.168.0.10\" }"
  D.input hostType dhall ≫ putStrLn ∘ show

  -- this requires 'Interpret' instance of Host
  (D.input auto dhall ∷ IO Host) ≫ putStrLn ∘ show

  let dhall2 = unlines [ "let HostsType = List { fqdn : Text, desc : Text"
                       , "                     , ipv4 : Text }"
                       , " in [ { fqdn = \"fqdn\", desc = \"descn\""
                       , "      , ipv4 = \"192.168.0.10\" } ]"
                       , "    : HostsType"
                       ]
  D.input hostMapType dhall2 ≫ putStrLn ∘ show

  let dhall3 = unlines [ "let HostsType = List { fqdn : Text, desc : Text"
                       , "                     , ipv4 : Text }"
                       , " in { hosts = [ { fqdn = \"fqdn\", desc = \"descn\""
                       , "                , ipv4 = \"192.168.0.10\" } ]"
                       , "    , aliases = [ { from = \"mailhost\", to = \"cargo\"} ] "
                       , "    , dns_servers = [ \"cargo\", \"chrome\" ]"
                       , "    , mail_servers = [ \"cargo\" ]"
                       , "    }"
--                       , "    : HostsType"
                       ]
  D.input hostsType dhall3 ≫ putStrLn ∘ show

----------------------------------------

__withTemps__ ∷ MonadIO μ ⇒ ((AbsFile,Handle) → (AbsFile,Handle) → IO α) → μ α
__withTemps__ io = let pfx1 = Just [pc|tinydns-data-|]
                       pfx2 = Just [pc|tinydns-data-.tmp|]
                    in (splitMError $ with2TempFiles' pfx1 pfx2 io) ≫ \ case
                       Left e → die (ExitFailure 4) e
                       Right r → return r


------------------------------------------------------------
--                       dhallisms                        --
------------------------------------------------------------

-- instance Parsecable IPv4 where
--   parser = IPv4 ⊳ _

word8 ∷ Stream s m Char ⇒ ParsecT s u m Word8
word8 = read ⊳ go
  where go = try (mappend ⊳ string "25" ⊵ (pure ⊳ oneOf "012345"))
           ∤ try ((:) ⊳ char '2' ⊵ ((:) ⊳ oneOf "01234" ⊵ count 1 digit))
           ∤ try ((:) ⊳ oneOf "01" ⊵ count 2 digit)
           ∤ try (count 2 digit)
           ∤ count 1 digit

ipv4P ∷ Stream s m Char ⇒ ParsecT s u m IP4
ipv4P = ip4FromOctets ⊳ word8 ⋪ char '.'
                      ⊵ word8 ⋪ char '.'
                      ⊵ word8 ⋪ char '.' ⊵ word8

ip4Type ∷ Type IPv4
ip4Type = Type {..}
  where extract (DC.TextLit (DC.Chunks [] t)) = case parse (ipv4P ⋪ eof) "" t of
                                                  Left  e  → error $ show e
                                                  Right ip → return (IPv4 ip)
        extract _                             = empty
        expected = DC.Text

instance Interpret IPv4 where
  autoWith _ = ip4Type

-- that's all, folks! ----------------------------------------------------------
