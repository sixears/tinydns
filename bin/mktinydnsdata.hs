{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE UnicodeSyntax       #-}

import Prelude ( error, undefined )

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import qualified  Data.Foldable  as  F

import Control.Exception  ( bracket )
import Control.Monad      ( fail, forM_, forM, join, mapM, mapM_, return )
import Data.Bool          ( Bool( True ), otherwise )
import Data.Bifunctor     ( bimap, first )
import Data.Char          ( Char, isAlphaNum )
import Data.Either        ( Either( Left, Right ), either, partitionEithers )
import Data.Eq            ( Eq )
import Data.Foldable      ( Foldable, concat )
import Data.Function      ( ($), (&), flip, id )
import Data.Functor       ( Functor, fmap )
import Data.List          ( (!!), intercalate, sortOn )
import Data.Maybe         ( Maybe( Just ), fromMaybe, maybe )
import Data.Ord           ( Ord, (<) )
import Data.String        ( IsString( fromString ), String )
import Data.Tuple         ( fst,snd )
import GHC.Generics       ( Generic )
import System.Exit        ( ExitCode( ExitFailure ) )
import System.IO          ( FilePath, Handle, IO, hClose, putStrLn )
import Text.Show          ( Show, show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , parseText, toString, toText )

-- fluffy ------------------------------

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
import Fluffy.Path           ( AbsDir, AbsFile, AsPathError, RelFile
                             , getCwd_, parseFile' )
import Fluffy.Path2          ( )
import Fluffy.TempFile2      ( pc, with2TempFiles' )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Getter  ( Getter, to )
import Control.Lens.Lens    ( Lens', lens )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- network-ip --------------------------

import Network.IP.Addr  ( IP4 )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, help )

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

import Data.Text     ( Text , all, null, pack, splitOn, unlines, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

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

------------------------------------------------------------

{- | hostname, no FQDN. -}
newtype UQDN = UQDN { unUQDN ∷ Text }
  deriving (Eq, Generic, Hashable, Ord, Show)

instance Printable UQDN where
  print (UQDN h) = P.text h

badShortHostName ∷ Text → String
badShortHostName t = "bad short host name: '" ⊕ unpack t ⊕ "'"

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
     in fmap HostMap ∘ fmap fromList ∘ mapM go $ toList hm
  parseJSON invalid     = typeMismatch "host map" invalid

hmHosts ∷ HostMap → [Host]
hmHosts (HostMap hm) = elems hm

------------------------------------------------------------

newtype ShortHostMap = ShortHostMap { unSHMap ∷ UQHMap UQDN }
  deriving Show

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

type Domain = Text
addAliasCmd ∷ AbsFile → AbsFile → Domain → Host → CmdSpec
addAliasCmd fn tmpfn d h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "alias", d, toText $ ipv4 h ]

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
    Right as → mapM_ runProc $ foldrWithKey ( \ u h a → addAliasCmd fn1 fn2 (toText u ⊕ "." ⊕ domains !! 0) h : a) [] as

  TextIO.readFile (toString fn1) ≫ TextIO.putStrLn

----------------------------------------

__withTemps__ ∷ MonadIO μ ⇒ ((AbsFile,Handle) → (AbsFile,Handle) → IO α) → μ α
__withTemps__ io = let pfx1 = Just [pc|tinydns-data-|]
                       pfx2 = Just [pc|tinydns-data-.tmp|]
                    in (splitMError $ with2TempFiles' pfx1 pfx2 io) ≫ \ case
                       Left e → die (ExitFailure 4) e
                       Right r → return r


-- that's all, folks! ----------------------------------------------------------
