{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

import Prelude ( error )

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( Object ), typeMismatch )

-- base --------------------------------

import Control.Monad        ( fail, forM_, forM, mapM, mapM_, return )
import Data.Bool            ( Bool( False, True ), not )
import Data.Bifunctor       ( first )
import Data.Either          ( Either( Left, Right ), either, partitionEithers )
import Data.Eq              ( Eq )
import Data.Function        ( ($), (&), const, flip, id )
import Data.Functor         ( fmap )
import Data.IORef           ( IORef, readIORef, writeIORef )
import Data.List            ( intercalate, sortOn )
import Data.Maybe           ( Maybe( Just, Nothing ), maybe )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import Data.Tuple           ( uncurry )
import GHC.Generics         ( Generic )
import System.Exit          ( ExitCode( ExitFailure ) )
import System.IO            ( FilePath, Handle, IO, putStrLn )
import Text.Show            ( Show, show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- dhall -------------------------------

import qualified  Dhall       as  D

import Dhall  ( Interpret( autoWith ), Type
              , auto, field, record )

-- domainnames -------------------------

import qualified  DomainNames.T.FQDN

import DomainNames.Error.DomainError     ( AsDomainError( _DomainError )
                                         , DomainError )
import DomainNames.Error.LocalnameError  ( LocalnameError )
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname, Localname, (<..>)
                                         , hostlocal, hostname, localname
                                         , parseLocalname'
                                         )

-- fluffy ------------------------------

import Fluffy.Applicative    ( (⊵) )
import Fluffy.Either         ( __right, leftFail )
import Fluffy.Foldable2      ( HasLength( length ) )
import Fluffy.Functor2       ( (⊳) )
import Fluffy.Lens2          ( (⊣), (⊢) )
import Fluffy.IO.Error2      ( )
import Fluffy.IP4            ( IP4, ip4 )
import Fluffy.Map            ( __fromList, fromList )
import Fluffy.Maybe          ( maybeE )
import Fluffy.Monad          ( (≫), returnPair )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadIO        ( MonadIO, liftIO )
import Fluffy.MonadIO2       ( die, dieUsage )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( AbsDir, AbsFile, RelFile
                             , extension, getCwd_, parseFile' )
import Fluffy.Path2          ( )
import Fluffy.Tasty          ( runTestsP_ )
import Fluffy.Tasty2         ( assertListEqIO )
import Fluffy.TempFile2      ( pc, with2TempFiles' )
import Fluffy.Text2          ( parenthesize )

-- hostsdb -----------------------------

import qualified  HostsDB.T.Host

import HostsDB.Host  ( Host( Host ), hname, hostType, ipv4 )

-- lens --------------------------------

import Control.Lens.Getter  ( Getter, to )
import Control.Lens.Lens    ( Lens', lens )
import Control.Lens.Prism   ( Prism', prism' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex, otoList )
                             )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as  OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, help )

-- parsec ------------------------------


-- path --------------------------------

import Path  ( File, Path, (</>), toFilePath )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError( ECExecE, ECCreateE ) )
import ProcLib.Process                ( mkProc_, runProcIO )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.RunProcOpts      ( defRunProcOpts, verboseL )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertFailure )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, concat, pack, unlines, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap
import Data.HashMap.Strict  ( HashMap, elems, lookup )

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml
import Data.Yaml  ( FromJSON( parseJSON ), Value( String )
                  , decodeEither' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  TinyDNS.Paths  as  Paths

--------------------------------------------------------------------------------

newtype HostMap = HostMap { unHostMap ∷ HashMap Localname Host }
  deriving (Eq, Show)

instance HasLength HostMap where
  length = length ∘ unHostMap

data LocalHostRelation = LocalHostRelation { lname ∷ Localname, lhost ∷ Host }
  deriving Eq

instance Printable LocalHostRelation where
  print lh = P.text ∘ parenthesize $ concat [ toText $ lname lh
                                            , " → "
                                            , toText $ lhost lh
                                            ]

type instance Element HostMap = LocalHostRelation
instance MonoFoldable HostMap where
  ofoldMap ∷ Monoid ξ ⇒ (LocalHostRelation → ξ) → HostMap → ξ
  ofoldMap f hm =
    HashMap.foldlWithKey' (\ a k v → a ⊕ f (LocalHostRelation k v)) ∅ hm

  ofoldr ∷ (LocalHostRelation → α → α) → α → HostMap → α
  ofoldr f init (HostMap hm) =
    HashMap.foldrWithKey (\ k v a → f (LocalHostRelation k v) a) init hm

  ofoldl' ∷ (α → LocalHostRelation → α) → α → HostMap → α
  ofoldl' f init (HostMap hm) =
    HashMap.foldlWithKey' (\ a k v → f a (LocalHostRelation k v)) init hm

  ofoldr1Ex ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
            → HostMap → LocalHostRelation
  ofoldr1Ex f (HostMap hm) =
    ofoldr1Ex f (uncurry LocalHostRelation ⊳ HashMap.toList hm)

  ofoldl1Ex' ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
             → HostMap → LocalHostRelation
  ofoldl1Ex' f (HostMap hm) =
    ofoldl1Ex' f (uncurry LocalHostRelation ⊳ HashMap.toList hm)


instance FromJSON HostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (Localname, Host)
        go (k,v@(Object _)) = returnPair (leftFail $ parseLocalname' k, parseJSON v)
        go (k,invalid)  =
          typeMismatch (unpack $ "Host: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ (mapM go $ HashMap.toList hm) ≫ \ case
          Left  dups → fail $ toString dups
          Right hm'  → return $ HostMap hm'
  parseJSON invalid = typeMismatch "host map" invalid

hmHosts ∷ HostMap → [Host]
hmHosts (HostMap hm) = elems hm

hostMapType ∷ Type HostMap
hostMapType = let localHNKey h = (hostlocal (hname h), h)
               in HostMap ∘ __fromList ∘ fmap localHNKey ⊳ D.list hostType

instance Interpret HostMap where
  autoWith _ = hostMapType

------------------------------------------------------------

newtype LocalHostMap = LocalHostMap { unSHMap ∷ HashMap Localname Localname }
  deriving (Eq, Show)

data LocalAlias = LocalAlias Localname Localname

localAliasType ∷ Type LocalAlias
localAliasType = record $ LocalAlias ⊳ field "from" auto
                                     ⊵ field "to"   auto

instance Interpret LocalAlias where
  autoWith _ = localAliasType

localAliasPair ∷ LocalAlias → (Localname,Localname)
localAliasPair (LocalAlias aliasFrom aliasTo) = (aliasFrom,aliasTo)

shortHostMapType ∷ Type LocalHostMap
shortHostMapType =
  LocalHostMap ⊳ __fromList ∘ fmap localAliasPair ⊳ D.list localAliasType

instance Interpret LocalHostMap where
  autoWith _ = shortHostMapType

instance FromJSON LocalHostMap where
  parseJSON (Object hm) =
    let go' ∷ MonadError LocalnameError η ⇒ (Text,Text) → η (Localname,Localname)
        go' (k, v) = do k' ← parseLocalname' k
                        v' ← parseLocalname' v
                        return (k',v')
        go ∷ (Text,Value) → Yaml.Parser (Localname, Localname)
        go (k,String v) = either (fail ∘ toString) return $ go' (k,v)
--          return (UQDN k, UQDN v)
        go (k,invalid)  =
          typeMismatch (unpack $ "short host name: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ (mapM go $ HashMap.toList hm) ≫ \ case
          Left dups → fail $ toString dups
          Right hm' → return $ LocalHostMap hm'
  parseJSON invalid     = typeMismatch "short host map" invalid

------------------------------------------------------------

data Hosts = Hosts { hosts        ∷ HostMap
                   , dns_servers  ∷ [Localname]
                   , mail_servers ∷ [Localname]
                   , aliases      ∷ LocalHostMap
                   }
  deriving (Eq, FromJSON, Generic)


hostsType ∷ Type Hosts
hostsType = record $ Hosts ⊳ field "hosts"        hostMapType
                           ⊵ field "dns_servers"  (D.list auto)
                           ⊵ field "mail_servers" (D.list auto)
                           ⊵ field "aliases"      shortHostMapType

instance Interpret Hosts where
  autoWith _ = hostsType


instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:        " ⊕ show (hosts h)
                            , "DNS_SERVERS:  " ⊕ show (dns_servers h)
                            , "MAIL_SERVERS: " ⊕ show (mail_servers h)
                            , "ALIASES:      " ⊕ show (aliases h)
                            ]

lookupHost ∷ Hosts → Localname → Maybe Host
lookupHost = flip lookup ∘ unHostMap ∘ hosts

hostIPv4 ∷ Hosts → Localname → Maybe IP4
hostIPv4 hs h = ipv4 ⊳ lookupHost hs h

hostIPv4' ∷ Hosts → Localname → Either Text IP4
hostIPv4' hs h = let quote t = "'" ⊕ toText t ⊕ "'"
                     noSuchH = "hostIPv4': no such host " ⊕ quote h
                 in maybe (Left noSuchH) Right $ hostIPv4 hs h

hostsHosts ∷ Hosts → [Host]
hostsHosts = hmHosts ∘ hosts

hostIPv4s ∷ Hosts → [(Hostname,IP4)]
hostIPv4s = fmap ( \ h → (hname h, ipv4 h) ) ∘ hostsHosts

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

__loadFileYaml__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileYaml__ fn = liftIO $
  decodeEither' ⊳ readFile (toFilePath fn) ≫ either (error ∘ show) return

__loadFileDhall__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileDhall__ fn = liftIO $
  Data.Text.IO.readFile (toFilePath fn) ≫ D.inputFrom (toFilePath fn) hostsType

domains ∷ [Text]
domains = ["sixears.co.uk", "0.168.192.in-addr.arpa"];

-- XXX
myDomain ∷ FQDN
myDomain = [fqdn|sixears.co.uk|]

----------------------------------------

addNSCmd ∷ AbsFile → AbsFile → IP4 → [CmdSpec]
addNSCmd fn tmpfn ip = ( \ d → CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "ns", toText d, toText ip ] ) ⊳ domains

----------------------------------------

addHostCmd ∷ AbsFile → AbsFile → Host → [CmdSpec]
addHostCmd fn tmpfn h = [ CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "host", toText $ hname h, toText $ ipv4 h ] ]

----------------------------------------

addAliasCmd ∷ AbsFile → AbsFile → Hostname → Host → CmdSpec
addAliasCmd fn tmpfn hname h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "alias", toText hname, toText $ ipv4 h ]

----------------------------------------

addMxCmd ∷ AbsFile → AbsFile → Host → CmdSpec
addMxCmd fn tmpfn h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "mx", toText (hname h), toText $ ipv4 h ]

----------------------------------------

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = runTestsP_ tests p

{- | like `withResource`, but with a no-op release resource -}
withResource' ∷ IO α → (IO α → TestTree) → TestTree
withResource' = flip withResource (const $ return ())

tests ∷ TestTree
tests = testGroup "mktinydnsdata" [ DomainNames.T.FQDN.tests
                                  , HostsDB.T.Host.tests
                                  , withResource' (D.input hostsType hostsTestText)
                                                  hostsDhallTests'
                                  ]

domainNamesTests ∷ TestTree
domainNamesTests = testGroup "DomainNames tests" [ DomainNames.T.FQDN.tests
                                                 , HostsDB.T.Host.tests ]

hostsTestHosts ∷ Hosts
hostsTestHosts =
  let chrome = Host [hostname|chrome.sixears.co.uk.|] [ip4|192.168.0.6|]
                "study desktop server" [ "fc:aa:14:87:cc:a2" ] Nothing
      winxp  = Host [hostname|winxp.sixears.co.uk.|] [ip4|192.168.0.87|]
                    "VirtualBox on Chrome" [ "08:00:27:23:08:43" ] Nothing
      cargo  = Host [hostname|cargo.sixears.co.uk.|] [ip4|192.168.0.9|]
                    "DVR" [ "e0:cb:4e:ba:be:60" ] Nothing
      expHostMap = HostMap $ HashMap.fromList [ ([localname|winxp|] , winxp)
                                              , ([localname|chrome|], chrome)
                                              , ([localname|cargo|] , cargo)
                                              ]
   in Hosts expHostMap [ [localname|cargo|], [localname|chrome|] ]
                       [ [localname|cargo|] ]
                       (LocalHostMap $ HashMap.fromList
                          [ ([localname|mailhost|], [localname|cargo|])
                          , ([localname|www|]     , [localname|chrome|])
                          , ([localname|cvs|]     , [localname|chrome|])
                          ]
                       )

hostsDhallTests' ∷ IO Hosts → TestTree
hostsDhallTests' hs =
  testGroup "hostsDhallTests" $ assertListEqIO "hosts"
                                              (otoList $ hosts hostsTestHosts)
                                              (otoList ∘ hosts ⊳ hs)
  
hostsTestText ∷ Text
hostsTestText =
  unlines [ "{ aliases = [ { from = \"mailhost\", to = \"cargo\"}"
          , "            , { from = \"www\",      to = \"chrome\"}"
          , "            , { from = \"cvs\",      to = \"chrome\"}"
          , "            ]"
          , ", dns_servers = [ \"cargo\", \"chrome\" ]"
          , ", mail_servers = [ \"cargo\" ]"
          , ""
          , ", hosts = [ { fqdn = \"chrome.sixears.co.uk.\""
          , "            , ipv4 = \"192.168.0.6\""
          , "            , desc = \"study desktop server\""
          , "            , mac= [ \"fc:aa:14:87:cc:a2\" ] : Optional Text"
          , "            , comments = [] : List Text"
          , "            }"
          , "          , { fqdn = \"winxp.sixears.co.uk.\""
          , "            , ipv4 = \"192.168.0.87\""
          , "            , desc = \"VirtualBox on Chrome\""
          , "            , mac= [ \"08:00:27:23:08:43\" ] : Optional Text"
          , "            , comments = [] : List Text"
          , "            }"
          , ""
          , "          , { fqdn = \"cargo.sixears.co.uk.\""
          , "            , ipv4 = \"192.168.0.9\""
          , "            , desc = \"DVR\""
          , "            , mac  = [ \"e0:cb:4e:ba:be:60\" ] : Optional Text"
          , "            , comments = [] : List Text"
          , "            }"
          , "          ]"
          , "}"
          ]

initIORef :: IORef Bool -> IO (IORef Bool)
initIORef ref = do
  v <- readIORef ref
  if v
    then assertFailure "resource was already initialized!"
    else writeIORef ref True
  return ref

releaseIORef :: IORef Bool -> IO ()
releaseIORef ref = do
  v <- readIORef ref
  if not v
    then assertFailure "resource was not initialized!"
  else writeIORef ref False

----------------------------------------

main ∷ IO ()
main = do
  let testText =
        unlines [ "{ aliases = [ { from = \"mailhost\", to = \"cargo\"}"
                , "            , { from = \"www\",      to = \"chrome\"}"
                , "            , { from = \"cvs\",      to = \"chrome\"}"
                , "            ]"
                , ", dns_servers = [ \"cargo\", \"chrome\" ]"
                , ", mail_servers = [ \"cargo\" ]"
                , ""
                , ", hosts = [ { fqdn = \"chrome.sixears.co.uk.\""
                , "            , ipv4 = \"192.168.0.6\""
                , "            , desc = \"study desktop server\""
                , "            , mac= [ \"fc:aa:14:87:cc:a2\" ] : Optional Text"
                , "            , comments = [] : List Text"
                , "            }"
                , "          , { fqdn = \"winxp.sixears.co.uk.\""
                , "            , ipv4 = \"192.168.0.87\""
                , "            , desc = \"VirtualBox on Chrome\""
                , "            , mac= [ \"08:00:27:23:08:43\" ] : Optional Text"
                , "            , comments = [] : List Text"
                , "            }"
                , ""
                , "          , { fqdn = \"cargo.sixears.co.uk.\""
                , "            , ipv4 = \"192.168.0.9\""
                , "            , desc = \"DVR\""
                , "            , mac  = [ \"e0:cb:4e:ba:be:60\" ] : Optional Text"
                , "            , comments = [] : List Text"
                , "            }"
                , "          ]"
                , "}"
                ]

  D.input hostsType testText ≫ putStrLn ∘ show

  cwd  ← getCwd_
  opts ← optParser "make tiny dns data from hosts config" (parseOptions cwd)
  let infn = opts ⊣ input
      ext  = infn ⊣ extension
  hs   ← case ext of
           ".yaml"  → __loadFileYaml__  (opts ⊣ input)
           ".dhall" → __loadFileDhall__ (opts ⊣ input)
           _      → dieUsage $ [fmtT|file ext not recognized: '%t'|] ext

  putStrLn (show hs)

  __withTemps__ $ __mkData__ hs

----------------------------------------

runProc ∷ (MonadIO μ, AsCreateProcError ε, AsExecError ε, MonadError ε μ) ⇒
          CmdSpec → μ ()
runProc = runProcIO (defRunProcOpts & verboseL ⊢ 1) ∘ mkProc_

runProc' ∷ CmdSpec → IO (Either ExecCreateError ())
runProc' = splitMError ∘ runProc

----------------------------------------

-- | Take a hashmap from α to Maybe β; and an error function; throw an error for any
--   `Nothing`s; otherwise return a hashmap from α to β.
-- allJusts ∷ MonadError ε η ⇒ (α → ε) → HashMap α (Maybe β) → η (HashMap α β)
-- allJusts err hm =

-- addAliasCmds ∷ AbsFile → AbsFile → FQDN → HashMap Localname Host
--              → IO (Either DomainError [Either ExecCreateError ()])
addAliasCmds ∷ (MonadIO μ, AsExecError ε, AsCreateProcError ε, AsDomainError ε,
                MonadError ε μ) ⇒
               AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds fn1 fn2 d as = mapM (\ (a,h) → a <..> d ≫ \ c → return $ addAliasCmd fn1 fn2 c h) (HashMap.toList as) ≫ mapM_ runProc

data ExecCreateDomainError = ECDExecCreateE ExecCreateError
                           | ECDDomainE     DomainError

instance Printable ExecCreateDomainError where
  print (ECDExecCreateE e) = P.string (show e)
  print (ECDDomainE e)     = print e

_ECDExecCreateE ∷ Prism' ExecCreateDomainError ExecCreateError
_ECDExecCreateE = prism' ECDExecCreateE (\ case (ECDExecCreateE e) → Just e; _ → Nothing)


instance AsExecError ExecCreateDomainError where
  _ExecError = prism' (ECDExecCreateE ∘ ECExecE)
                      (\ case (ECDExecCreateE (ECExecE e)) → Just e
                              _                              → Nothing)
instance AsCreateProcError ExecCreateDomainError where
  _CreateProcError = prism' (ECDExecCreateE ∘ ECCreateE)
                            (\ case (ECDExecCreateE (ECCreateE e)) → Just e
                                    _                                → Nothing)

instance AsDomainError ExecCreateDomainError where
  _DomainError = prism' ECDDomainE
                        (\ case (ECDDomainE e) → Just e; _ → Nothing)

addAliasCmds' ∷ (MonadIO μ, MonadError ExecCreateDomainError μ) ⇒
                AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds' = addAliasCmds

addAliasCmds''  ∷ MonadIO μ ⇒
                  AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds'' fn1 fn2 d as = __right ⊳ (splitMError $ addAliasCmds' fn1 fn2 d as)

__mkData__ ∷ Hosts → (AbsFile, _z0) → (AbsFile, _z1) → IO ()
__mkData__ hs (fn1,_) (fn2,_) = do
  case partitionEithers $ ( \ h → hostIPv4' hs h) ⊳ dns_servers hs of
    ([], ips) → do forM_ ips $ \ ip → forM_ (addNSCmd fn1 fn2 ip) $ runProc'

    (es, _)   → die (ExitFailure 3) (unlines es)

  forM_ (addHostCmd fn1 fn2 ⊳ (sortOn ipv4 $ hostsHosts hs)) (mapM_ runProc')

  case forM (unSHMap $ aliases hs) ( \ h → maybeE h (lookupHost hs h) ) of
    Left  h  → die (ExitFailure 3) h
    Right as → addAliasCmds'' fn1 fn2 myDomain as

  case forM (( \ h → (h,lookupHost hs h)) ⊳ mail_servers hs) ( \ (hn,mh) → maybeE hn mh) of
    Left  h → die (ExitFailure 3) h
    Right as → mapM_ runProc' $ ( addMxCmd fn1 fn2 ) ⊳ as


  Data.Text.IO.readFile (toString fn1) ≫ Data.Text.IO.putStrLn

  let dhall = "{ fqdn = \"fqdn\", desc = \"descn\", ipv4 = \"192.168.0.10\""
            ⊕ ", mac = [] : Optional Text, comments = [] : List Text }"
  D.input hostType dhall ≫ putStrLn ∘ show

  -- this requires 'Interpret' instance of Host
  (D.input auto dhall ∷ IO Host) ≫ putStrLn ∘ show

  let dhall2 = unlines [ "let HostsType = List { fqdn : Text"
                       , "                     , desc : Text"
                       , "                     , ipv4 : Text"
                       , "                     , comments : List Text"
                       , "                     , mac : Optional Text"
                       , "                     }"
                       , " in [ { fqdn = \"fqdn\""
                       , "      , desc = \"descn\""
                       , "      , ipv4 = \"192.168.0.10\""
                       , "      , comments = [] : List Text"
                       , "      , mac = [] : Optional Text"
                       , "      } ]"
                       , "    : HostsType"
                       ]
  D.input hostMapType dhall2 ≫ putStrLn ∘ show

  let dhall3 = unlines [ "let HostsType = List { fqdn     : Text"
                       , "                     , desc     : Text"
                       , "                     , ipv4     : Text"
                       , "                     , comments : List Text"
                       , "                     , mac      : Optional Text"
                       , "                     }"
                       , " in { hosts = [ { fqdn = \"fqdn\""
                       , "                , desc = \"descn\""
                       , "                , ipv4 = \"192.168.0.10\""
                       , "                , comments = [] : List Text"
                       , "                , mac = [] : Optional Text"
                       , "                }"
                       , "              ]"
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

-- that's all, folks! ----------------------------------------------------------
