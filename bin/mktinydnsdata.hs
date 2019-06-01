{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

-- !!! Cleanup Temp Files !!!
-- !!! REWRITE Fluffy.TempFile TO NOT USE LAZY IO !!!

import Prelude ( error )

-- base --------------------------------

import Control.Monad        ( forM_, forM, mapM, mapM_, return, when )
import Data.Bifunctor       ( first )
import Data.Bool            ( Bool )
import Data.Either          ( Either( Left, Right ), either, partitionEithers )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, toList )
import Data.Function        ( ($), (&), id )
import Data.Functor         ( fmap )
import Data.List            ( sortOn )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just ) )
import Data.Monoid          ( Monoid( mconcat, mempty ) )
import Data.Semigroup       ( Semigroup( (<>) ) )
import Data.String          ( String )
import Data.Tuple           ( fst, snd, swap, uncurry )
import System.Exit          ( ExitCode( ExitFailure ) )
import System.IO            ( Handle, IO, hClose )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- containers --------------------------

import qualified  Data.Map  as  Map
import Data.Map  ( mapAccumWithKey )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- dhall -------------------------------

import Dhall  ( auto, defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- domainnames -------------------------

import DomainNames.Domain                ( IsDomainLabels( dLabels ) )
import DomainNames.Error.DomainError     ( AsDomainError )
import DomainNames.Error.ExecCreateDomainError ( ExecCreateDomainError )
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname, Localname
                                         , (<..>), hostname, localname )

-- fluffy ------------------------------

import Fluffy.Containers.NonEmptyHashSet
                             ( NonEmptyHashSet, toNEList )
import Fluffy.IP4            ( IP4, ip4 )
import Fluffy.MACAddress     ( mac )
import Fluffy.MapUtils       ( fromListWithDups )
import Fluffy.Maybe          ( maybeE )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadIO        ( MonadIO, die, dieUsage, liftIO, unlink_ )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( AbsDir, AbsFile, RelFile
                             , extension, getCwd_, parseAbsFile_, parseFile' )
import Fluffy.TempFile       ( pc, with2TempFiles' )

-- hostsdb -----------------------------

import HostsDB.Host          ( Host( Host ), hname, ipv4 )
import HostsDB.Hosts         ( Hosts( Hosts ), aliases, dns_servers, domain
                             , hostIPv4', hostIPv4s, lookupHost
                             , mail_servers
                             )
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

import ProcLib.CommonOpt.DryRun       ( DryRun, HasDryRunLevel( dryRunLevel )
                                      , dryRunOn, dryRunP )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel ), Verbose
                                      , verboseOn, verboseP )
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

import Text.Fmt  ( fmt, fmtT )

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

data Clean = Clean | NoClean            deriving (Eq, Show)
data TestMode = TestMode | NoTestMode

------------------------------------------------------------

data Options = Options { _dryRun   ∷ DryRun
                       , _verbose  ∷ Verbose
                       , _input    ∷ AbsFile
                       , _clean    ∷ Clean
                       , _testMode ∷ TestMode
                       }

input ∷ Lens' Options AbsFile
input = lens _input (\ o i → o { _input = i })

clean ∷ Lens' Options Clean
clean = lens _clean (\ o d → o { _clean = d })

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
   in Options ⊳ dryRunP
              ⊵ verboseP
              ⊵ argument (readAbsFile cwd) (metavar "HOSTS.dhall")
              ⊵ flag Clean NoClean (long "no-clean" ⊕ help helpText)
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

addHostCmd ∷ AbsFile → AbsFile → Hostname → IP4 → [CmdSpec]
addHostCmd fn tmpfn hn ip = [ CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "host", toText hn, toText ip ] ]

----------------------------------------

addAliasCmd ∷ AbsFile → AbsFile → Host → Hostname → CmdSpec
addAliasCmd fn tmpfn h name = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "alias", toText name, toText $ h ⊣ ipv4 ]

----------------------------------------

addMxCmd ∷ AbsFile → AbsFile → Host → CmdSpec
addMxCmd fn tmpfn h = CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "mx", toText (h ⊣ hname), toText $ h ⊣ ipv4 ]

----------------------------------------

testHosts ∷ Hosts
testHosts =
  let lfoo          = [localname|foo|]
      lfoowl        = [localname|foow-l|]
      lbaz          = [localname|baz|]
      dmn           = [fqdn|sixears.co.uk.|]
      foohost       = Host [hostname|foo.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|01:23:45:67:89:0A|])
      foowlhost     = Host [hostname|foo-wl.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|0A:89:67:45:23:01|])
      hosts         = LHostMap $ HashMap.fromList [ (lfoo,foohost)
                                                  , (lfoowl,foowlhost)]
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


  __with2Temps__ (opts ⊣ clean ≡ NoClean) $ __mkData__ hs

----------------------------------------

runProc ∷ (MonadIO μ, AsCreateProcError ε, AsExecError ε, MonadError ε μ) ⇒
          CmdSpec → μ ()
runProc c = runProcIO (defRunProcOpts & verboseL ⊢ 1) $ mkProc_ c

__runProc__ ∷ MonadIO μ ⇒ CmdSpec → μ ()
__runProc__ = exceptIOThrow ∘ runProc @_ @ExecCreateError

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

newtype ErrTs = ErrTs [Text]

toTexts ∷ ErrTs → [Text]
toTexts (ErrTs ts) = ts

errT ∷ Text → ErrTs
errT t = ErrTs [t]

instance Semigroup ErrTs where
  (ErrTs es) <> (ErrTs es') = ErrTs (es ⊕ es')

instance Monoid ErrTs where
  mempty = ErrTs []

ф ∷ Monoid α ⇒ α
ф = mempty
ю ∷ (Foldable φ, Monoid α) ⇒ φ α → α
ю = mconcat ∘ toList

__mkData__ ∷ Hosts → AbsFile → AbsFile → IO ()
__mkData__ hs fn1 fn2 = do
  case partitionEithers $ hostIPv4' hs ⊳ hs ⊣ dns_servers of
    ([], ips) → forM_ ips $ \ ip → forM_ (addNSCmd fn1 fn2 ip) __runProc__

    (es, _)   → die (ExitFailure 3) (unlines es)

  let dupIPHosts ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
      hostsByIP  ∷ Map.Map IP4 Hostname
      (dupIPHosts, hostsByIP) = fromListWithDups $ swap ⊳ hostIPv4s hs

      -- given two hostnames; if one is the other+"-wl", then return the base
      -- name - else barf
      checkWL' ∷ ErrTs → Hostname → Hostname → (ErrTs,Hostname)
      checkWL' es h1 h2 = let (l1 :| d1) = h1 ⊣ dLabels
                              (l2 :| d2) = h2 ⊣ dLabels
                              errNm = [fmt|names are not a+wl: '%T' vs. '%T'|] h1 h2
                              errDm = [fmt|different domains: '%T' vs. '%T'|] h1 h2
                           in if d1 ≡ d2
                              then if toText l1 ≡ toText l2 ⊕ "-wl"
                                   then (es,h2)
                                   else if toText l2 ≡ toText l1 ⊕ "-wl"
                                        then (es,h1)
                                        else (es ⊕ errT errNm,h1)
                              else (es ⊕ errT errDm,h1)
      -- check that ip4, hostnames is a pair of hostnames where one
      -- is the other + "-wl"; return the base name
      checkWL ∷ ErrTs → IP4 → NonEmptyHashSet Hostname → (ErrTs, Hostname)
      checkWL es i hh = let errTooMany l = [fmt|too many hosts for IP %T (%L)|] i l
                         in case toNEList hh of
                              h  :| []     → (es,h)
                              h1 :| [h2]   → checkWL' es h1 h2
                              lh@(h1 :| _) → (es ⊕ errT (errTooMany lh), h1)
      -- check that the map ip4 -> hostnames has only pairs of hostnames where one
      -- is the other + "-wl"; return the base name in each case
      filterWL ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
               → (Map.Map IP4 Hostname, ErrTs)
      filterWL = swap ∘ mapAccumWithKey checkWL ф
  let hostList ∷ [(Hostname,IP4)]
      es         ∷ ErrTs
      (hostList,es) = let (filteredDups,es') = filterWL dupIPHosts
                          hostList' = ю [ swap ⊳ Map.toList hostsByIP
                                        , swap ⊳ Map.toList filteredDups ]
                       in (hostList', es')
  forM_ (toTexts es) $ putStrLn ∘ ("!ERROR: " ⊕) 
  forM_ ((\ h → addHostCmd fn1 fn2 (fst h) (snd h)) ⊳ sortOn snd hostList)
        (mapM_ __runProc__)

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

__with2Temps__ ∷ MonadIO μ ⇒ Bool → (AbsFile → AbsFile → IO α) → μ α
__with2Temps__ cl io = do
-- BEWARE THE LAZY IO!
-- REWRITE TEMP TO AVOID ALL SYSTEM.IO io (that is, lazy io)
  (tmpfn1, h1) ← liftIO $ mkstemps "/tmp/tinydns-data-" ""
  (tmpfn2, h2) ← liftIO $ mkstemps "/tmp/tinydns-data-" ".tmp"
  let tmp1 = parseAbsFile_ tmpfn1
      tmp2 = parseAbsFile_ tmpfn2
  liftIO $ hClose h1
  liftIO $ hClose h2
  r ← liftIO $ io tmp1 tmp2
  liftIO $ when cl $ unlink_ tmp1 -- ⪼ unlink_ tmp2 -- tmp2 is auto-deleted
                                                        -- by tinydns-data
  return r

-- that's all, folks! ----------------------------------------------------------
