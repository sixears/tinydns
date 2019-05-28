{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- !!! REWRITE Fluffy.TempFile TO NOT USE LAZY IO !!!

import Prelude ( error )

-- base --------------------------------

import Control.Monad        ( forM_, forM, mapM, mapM_, return )
import Data.Bifunctor       ( first )
import Data.Either          ( Either( Left, Right ), either, partitionEithers )
import Data.Function        ( ($), (&), id )
import Data.Functor         ( fmap )
import Data.List            ( sortOn )
import Data.Maybe           ( Maybe( Just ) )
import Data.String          ( String )
import Data.Tuple           ( uncurry )
import System.Exit          ( ExitCode( ExitFailure ) )
import System.IO            ( Handle, IO, hClose )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- dhall -------------------------------

import Dhall  ( auto, defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- domainnames -------------------------

import DomainNames.Error.DomainError     ( AsDomainError )
import DomainNames.Error.ExecCreateDomainError ( ExecCreateDomainError )
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname, Localname
                                         , (<..>), hostname, localname )

-- fluffy ------------------------------

import Fluffy.IP4            ( IP4, ip4 )
import Fluffy.MACAddress     ( mac )
import Fluffy.Maybe          ( maybeE )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadIO        ( MonadIO, die, dieUsage, liftIO )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( AbsDir, AbsFile, RelFile
                             , extension, getCwd_, parseAbsFile_, parseFile' )
import Fluffy.TempFile       ( pc, with2TempFiles' )

-- hostsdb -----------------------------

import HostsDB.Host          ( Host( Host ), hname, ipv4 )
import HostsDB.Hosts         ( Hosts( Hosts ), aliases, dns_servers, domain
                             , hostsHosts, hostIPv4', lookupHost, mail_servers )
import HostsDB.LHostMap      ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap  ( LocalnameMap( LocalnameMap ), unLHMap )

-- lens --------------------------------

import Control.Lens.Lens     ( Lens', lens )
import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as  OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, flag, help
                                    , long, metavar )

-- path --------------------------------

import Path  ( File, Path, (</>), toFilePath )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Error.ExecCreateError  ( ExecCreateError )
import ProcLib.Process                ( mkProc_, runProcIO )
import ProcLib.Types.CmdSpec          ( CmdSpec( CmdSpec ) )
import ProcLib.Types.RunProcOpts      ( defRunProcOpts, verboseL )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, pack, unlines )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

-- unix --------------------------------

import System.Posix.Temp  ( mkstemps )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap
import Data.HashMap.Strict  ( HashMap )

-- yaml --------------------------------

import Data.Yaml  ( decodeEither' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  TinyDNS.Paths  as  Paths

--------------------------------------------------------------------------------

------------------------------------------------------------

data Debug = Debug | NoDebug
data TestMode = TestMode | NoTestMode

------------------------------------------------------------

data Options = Options { _input    ∷ AbsFile
                       , _debug    ∷ Debug
                       , _testMode ∷ TestMode
                       }

input ∷ Lens' Options AbsFile
input = lens _input (\ o i → o { _input = i })

debug ∷ Lens' Options Debug
debug = lens _debug (\ o d → o { _debug = d })

testMode ∷ Lens' Options TestMode
testMode = lens _testMode (\ o t → o { _testMode = t })

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
parseOptions cwd =
  let helpText = "don't delete intermediate files"
   in Options ⊳ argument (readAbsFile cwd) (metavar "HOSTS.dhall")
              ⊵ flag NoDebug Debug (long "debug" ⊕ help helpText)
              ⊵ flag NoTestMode TestMode (long "test" ⊕ help "use test data")

-- | Perform some IO within a temporary directory freshly created by `mkTempDir`.
--   Cleans away the created directory when IO is complete.

__loadFileYaml__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileYaml__ fn = liftIO $
  decodeEither' ⊳ readFile (toFilePath fn) ≫ either (error ∘ show) return

__loadFileDhall__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileDhall__ fn = liftIO $
--  Data.Text.IO.readFile (toFilePath fn) ≫ D.inputFrom (toFilePath fn) auto
    Data.Text.IO.readFile (toFilePath fn) ≫ inputWithSettings (defaultInputSettings & sourceName ⊢ toFilePath fn & rootDirectory ⊢ toFilePath fn ⊣ directory) auto

domains ∷ [Text]
domains = ["sixears.co.uk", "0.168.192.in-addr.arpa"];

-- XXX
myDomain ∷ FQDN
myDomain = [fqdn|sixears.co.uk.|]

----------------------------------------

addNSCmd ∷ AbsFile → AbsFile → IP4 → [CmdSpec]
addNSCmd fn tmpfn ip = ( \ d → CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "ns", toText d, toText ip ] ) ⊳ domains

----------------------------------------

addHostCmd ∷ AbsFile → AbsFile → Host → [CmdSpec]
addHostCmd fn tmpfn h = [ CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "host", toText $ hname h, toText $ ipv4 h ] ]

----------------------------------------

addAliasCmd ∷ AbsFile → AbsFile → Host → Hostname → CmdSpec
addAliasCmd fn tmpfn h name = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "alias", toText name, toText $ ipv4 h ]

----------------------------------------

addMxCmd ∷ AbsFile → AbsFile → Host → CmdSpec
addMxCmd fn tmpfn h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "mx", toText (hname h), toText $ ipv4 h ]

----------------------------------------

testHosts ∷ Hosts
testHosts =
  let lfoo          = [localname|foo|]
      lbaz          = [localname|baz|]
      dmn           = [fqdn|sixears.co.uk.|]
      foohost       = Host [hostname|bar.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|01:23:45:67:89:0A|])
      hosts         = LHostMap $ HashMap.fromList [(lfoo,foohost)]
      dns_servers_  = [lfoo]
      mail_servers_ = [lfoo]
      aliases_      = LocalnameMap $ HashMap.fromList [(lbaz,lfoo)]
   in Hosts dmn hosts dns_servers_ mail_servers_ aliases_

main ∷ IO ()
main = do
  cwd  ← getCwd_
  opts ← optParser "make tiny dns data from hosts config" (parseOptions cwd)

  let infn = opts ⊣ input
      ext  = infn ⊣ extension
  hs   ← case opts ⊣ testMode of
           TestMode   → return testHosts
           NoTestMode → let badExt = [fmtT|file ext not recognized: '%t'|] ext
                         in case ext of 
                              ".yaml"  → __loadFileYaml__  (opts ⊣ input)
                              ".dhall" → __loadFileDhall__ (opts ⊣ input)
                              _        → dieUsage badExt


--  __withTemps__ $ __mkData__ hs
  __with2Temps__ $ __mkData'__ hs

----------------------------------------

runProc ∷ (MonadIO μ, AsCreateProcError ε, AsExecError ε, MonadError ε μ) ⇒
          CmdSpec → μ ()
runProc c = runProcIO (defRunProcOpts & verboseL ⊢ 1) $ mkProc_ c

runProc' ∷ CmdSpec → IO (Either ExecCreateError ())
runProc' = splitMError ∘ runProc

runProc'' ∷ (MonadIO μ, MonadError ExecCreateError μ) ⇒
            CmdSpec → μ ()
runProc'' = runProc

__runProc__ ∷ MonadIO μ ⇒ CmdSpec → μ ()
__runProc__ = exceptIOThrow ∘ runProc''

----------------------------------------

addAliasCmds ∷ (MonadIO μ, AsExecError ε, AsCreateProcError ε, AsDomainError ε,
                MonadError ε μ) ⇒
               AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds fn1 fn2 d as =
  let go (a,h) = a <..> d ≫ \ x → return $ addAliasCmd fn1 fn2 h x
  in mapM go (HashMap.toList as) ≫ \ x → mapM_ runProc x

addAliasCmds' ∷ (MonadIO μ, MonadError ExecCreateDomainError μ) ⇒
                AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds' = addAliasCmds

addAliasCmds''  ∷ MonadIO μ ⇒
                  AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds'' fn1 fn2 d as = exceptIOThrow $ addAliasCmds' fn1 fn2 d as

__mkData__ ∷ Hosts → (AbsFile, Handle) → (AbsFile, Handle) → IO ()
__mkData__ hs (fn1,h1) (fn2,h2) = do
  hClose h1
  hClose h2
  __mkData'__ hs fn1 fn2

__mkData'__ ∷ Hosts → AbsFile → AbsFile → IO ()
__mkData'__ hs fn1 fn2 = do
  case partitionEithers $ hostIPv4' hs ⊳ hs ⊣ dns_servers of
    ([], ips) → forM_ ips $ \ ip → forM_ (addNSCmd fn1 fn2 ip) __runProc__

    (es, _)   → die (ExitFailure 3) (unlines es)

  forM_ (addHostCmd fn1 fn2 ⊳ sortOn ipv4 (hostsHosts hs)) (mapM_ __runProc__)

  case forM (unLHMap $ hs ⊣ aliases) ( \ h → maybeE h (lookupHost hs h) ) of
    Left  h  → die (ExitFailure 3) h
    Right as → addAliasCmds'' fn1 fn2 (hs ⊣ domain) as

  case forM (( \ h → (h,lookupHost hs h)) ⊳ hs ⊣ mail_servers ) (uncurry maybeE)  of
    Left  h → die (ExitFailure 3) h
    Right as → mapM_ __runProc__ $ addMxCmd fn1 fn2 ⊳ as


  Data.Text.IO.readFile (toString fn1) ≫ putStrLn

----------------------------------------

__withTemps__ ∷ MonadIO μ ⇒ ((AbsFile,Handle) → (AbsFile,Handle) → IO α) → μ α
__withTemps__ io = let pfx1 = Just [pc|tinydns-data-|]
                       pfx2 = Just [pc|tinydns-data-.tmp|]
                    in splitMError (with2TempFiles' pfx1 pfx2 io) ≫ \ case
                       Left e → die (ExitFailure 4) e
                       Right r → return r

__with2Temps__ ∷ MonadIO μ ⇒ (AbsFile → AbsFile → IO α) → μ α
__with2Temps__ io = do
-- BEWARE THE LAZY IO!
-- REWRITE TEMP TO AVOID ALL SYSTEM.IO io (that is, lazy io)
{-
  (tmpfn1, h1) ← liftIO $ mkstemps "tinydns-data-" ""
  (tmpfn2, h2) ← liftIO $ mkstemps "tinydns-data-" ".tmp"
  liftIO $ hClose h1
  liftIO $ hClose h2
  splitMError (asIOError $ io tmpfn1 tmpfn2) ≫ \ case
    Left e → die (ExitFailure 4) e
    Right r → return r
-}  
  (tmpfn1, h1) ← liftIO $ mkstemps "/tmp/tinydns-data-" ""
  (tmpfn2, h2) ← liftIO $ mkstemps "/tmp/tinydns-data-" ".tmp"
  liftIO $ hClose h1
  liftIO $ hClose h2
  liftIO $ io (parseAbsFile_ tmpfn1) (parseAbsFile_ tmpfn2)

-- that's all, folks! ----------------------------------------------------------
