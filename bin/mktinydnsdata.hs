{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE UnicodeSyntax       #-}

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
import System.Exit          ( ExitCode( ExitFailure ) )
import System.IO            ( Handle, IO, putStrLn )
import Text.Show            ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- dhall -------------------------------

import qualified  Dhall       as  D

import Dhall  ( auto )

-- domainnames -------------------------

import DomainNames.Error.DomainError     ( AsDomainError )
import DomainNames.Error.ExecCreateDomainError ( ExecCreateDomainError )
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname, Localname, (<..>) )

-- fluffy ------------------------------

import Fluffy.Either      ( __right )
import Fluffy.Functor     ( (⊳) )
import Fluffy.Lens        ( (⊣), (⊢) )
import Fluffy.IP4         ( IP4 )
import Fluffy.Maybe       ( maybeE )
import Fluffy.Monad       ( (≫) )
import Fluffy.MonadError  ( splitMError )
import Fluffy.MonadIO     ( MonadIO, die, dieUsage, liftIO )
import Fluffy.Options     ( optParser )
import Fluffy.Path        ( AbsDir, AbsFile, RelFile
                          , extension, getCwd_, parseFile' )
import Fluffy.TempFile    ( pc, with2TempFiles' )

-- hostsdb -----------------------------

import HostsDB.Host          ( Host, hname, ipv4 )
import HostsDB.Hosts         ( Hosts, aliases, dns_servers, hostsHosts
                             , hostIPv4', lookupHost, mail_servers )
import HostsDB.LocalnameMap  ( unLHMap )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as  OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, help )

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

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

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

------------------------------------------------------------

data Options = Options { _input ∷ AbsFile }

input ∷ Lens' Options AbsFile
input = lens _input ( \ o i → o { _input = i } )

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
  Data.Text.IO.readFile (toFilePath fn) ≫ D.inputFrom (toFilePath fn) auto

domains ∷ [Text]
domains = ["sixears.co.uk", "0.168.192.in-addr.arpa"];

-- XXX
myDomain ∷ FQDN
myDomain = [fqdn|sixears.com.uk.|]

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

main ∷ IO ()
main = do
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

addAliasCmds ∷ (MonadIO μ, AsExecError ε, AsCreateProcError ε, AsDomainError ε,
                MonadError ε μ) ⇒
               AbsFile → AbsFile → FQDN → HashMap Localname Host → μ ()
addAliasCmds fn1 fn2 d as =
  let go (a,h) = a <..> d ≫ return ∘ addAliasCmd fn1 fn2 h
  in mapM go (HashMap.toList as) ≫ mapM_ runProc

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

  case forM (unLHMap $ aliases hs) ( \ h → maybeE h (lookupHost hs h) ) of
    Left  h  → die (ExitFailure 3) h
    Right as → addAliasCmds'' fn1 fn2 myDomain as

  case forM (( \ h → (h,lookupHost hs h)) ⊳ mail_servers hs) ( \ (hn,mh) → maybeE hn mh) of
    Left  h → die (ExitFailure 3) h
    Right as → mapM_ runProc' $ ( addMxCmd fn1 fn2 ) ⊳ as


  Data.Text.IO.readFile (toString fn1) ≫ Data.Text.IO.putStrLn

----------------------------------------

__withTemps__ ∷ MonadIO μ ⇒ ((AbsFile,Handle) → (AbsFile,Handle) → IO α) → μ α
__withTemps__ io = let pfx1 = Just [pc|tinydns-data-|]
                       pfx2 = Just [pc|tinydns-data-.tmp|]
                    in (splitMError $ with2TempFiles' pfx1 pfx2 io) ≫ \ case
                       Left e → die (ExitFailure 4) e
                       Right r → return r

-- that's all, folks! ----------------------------------------------------------
