{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- !!! Cleanup Temp Files !!!
-- !!! REWRITE Fluffy.TempFile TO NOT USE LAZY IO !!!

import Prelude ( error )

-- base --------------------------------

import Control.Monad       ( foldM, forM_, mapM, mapM_, return, when )
import Data.Bifunctor      ( first )
import Data.Either         ( Either( Left, Right ), either )
import Data.Eq             ( Eq )
import Data.Foldable       ( Foldable, toList )
import Data.Function       ( ($), (&), flip, id )
import Data.Functor        ( Functor, fmap )
import Data.List           ( sortOn )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just ) )
import Data.Monoid         ( Monoid( mconcat, mempty ) )
import Data.Semigroup      ( Semigroup( (<>) ) )
import Data.String         ( String )
import Data.Tuple          ( snd, swap, uncurry )
import System.Exit         ( ExitCode( ExitFailure ) )
import System.IO           ( Handle, IO, hClose )
import Text.Show           ( Show( show ) )

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
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname
                                         , (<..>), hostname, localname )

-- fluffy ------------------------------

import Fluffy.Containers.NonEmptyHashSet
                             ( NonEmptyHashSet, toNEList )
import Fluffy.Functor        ( (<$$>) )
import Fluffy.IP4            ( IP4, ip4 )
import Fluffy.MACAddress     ( mac )
import Fluffy.MapUtils       ( fromListWithDups )
import Fluffy.MonadError     ( splitMError )
import Fluffy.MonadIO        ( MonadIO, die, dieUsage, liftIO, unlink_ )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Nat            ( One )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( AbsDir, AbsFile, RelFile
                             , extension, getCwd_, parseAbsFile_, parseFile' )
import Fluffy.TempFile       ( pc, with2TempFiles' )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( AsHostsError, HostsDomainExecCreateError )
import HostsDB.Host              ( Host( Host ), hname, ipv4 )
import HostsDB.Hosts             ( Hosts( Hosts )
                                 , aliasHosts, dnsServers, domain, hostIPv4
                                 , hostIPv4s, lookupHost, mailServers
                                 )
import HostsDB.LHostMap          ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap      ( LocalnameMap( LocalnameMap ) )

-- lens --------------------------------

import Control.Lens.Lens     ( Lens', lens )
import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )
import Control.Monad.Reader  ( MonadReader, ReaderT, ask, runReaderT )
import Control.Monad.Trans   ( MonadTrans, lift )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as  OptParse

import Options.Applicative.Builder  ( ReadM, argument, eitherReader, flag, help
                                    , long, metavar )

-- path --------------------------------

import Path  ( File, Path, (</>), absfile, toFilePath )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun       ( DryRun, HasDryRunLevel( dryRunLevel )
                                      , dryRunOn, dryRunP )
import ProcLib.CommonOpt.Verbose      ( HasVerboseLevel( verboseLevel ), Verbose
                                      , verboseOn, verboseP )
import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Error.ExecError        ( AsExecError )
import ProcLib.Error.ExecCreateError  ( ExecCreateError )
import ProcLib.Process                ( doProcIO, mkIO, mkIO', mkProc_, runProcIO, system )
import ProcLib.Types.CmdSpec          ( CmdArgs, CmdSpec( CmdSpec ) )
import ProcLib.Types.CreateProcOpts   ( MockLvl( MockLvl ), defCPOpts )
import ProcLib.Types.ProcIO           ( ProcIO, ProcIO' )
import ProcLib.Types.RunProcOpts      ( defRunProcOpts, verboseL )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, pack )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unix --------------------------------

import System.Posix.Temp  ( mkstemps )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

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
{- | just for ghci testing -}
_defaultOptions ∷ Options
_defaultOptions =
  Options dryRunOn verboseOn
          [absfile|/home/martyn/rc/nixos/hostcfg/sixears-hosts.dhall|]
          Clean TestMode

dryRun ∷ Lens' Options DryRun
dryRun = lens _dryRun (\ o d → o { _dryRun = d })

instance HasDryRunLevel One Options where
  dryRunLevel = dryRun

verbose ∷ Lens' Options Verbose
verbose = lens _verbose (\ o v → o { _verbose = v })

instance HasVerboseLevel One Options where
  verboseLevel = verbose

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

----------------------------------------

type TmpFiles = (AbsFile,AbsFile) -- (output file, tinydns intermediar)
type TmpFilesReader η = MonadReader TmpFiles η

mkNSCmd ∷ (Foldable ψ, Functor ψ,
               AsCreateProcError ε, AsExecError ε, MonadError ε η,
               MonadTrans τ, MonadReader (AbsFile, AbsFile) (τ (ProcIO' ε η))) ⇒
              ψ FQDN → IP4 → τ (ProcIO' ε η) ()
mkNSCmd domains ip = do
  (fn,tmpfn) ← ask
  lift $ mapM_ (mkProc_ @_ @()) (addNSCmd fn tmpfn domains ip)

mkNSCmd' ∷ (Foldable ψ, AsCreateProcError ε, AsExecError ε,
            MonadIO μ, MonadReader Clean μ) ⇒
           Text → ψ FQDN → IP4 → ProcIO ε μ Text
mkNSCmd' intxt domains ip = do
  foldM (\ t d → tinydnsEdit' t [ "add", "ns", toText d, toText ip ]) intxt domains

tinydnsEdit ∷ [Text] → CmdSpec
tinydnsEdit args = CmdSpec Paths.tinydns_edit args

tinydnsEdit' ∷ (AsCreateProcError ε, AsExecError ε,
                MonadIO μ, MonadReader Clean μ) ⇒
               Text → CmdArgs → ProcIO ε μ Text

tinydnsEdit' intxt args = do
  cl ← lift $ ask
  let mock1 = [absfile|/tmp/mktinydns.data|]
      mock2 = [absfile|/tmp/mktinydns.data.tmp|]
  (tmp1,tmp2) ← mkIO' (MockLvl 1) (mock1,mock2) "mktmpnames" $ liftIO $ do
    (tmpfn1, h1) ← mkstemps "/tmp/tinydns-data-" ""
    (tmpfn2, h2) ← mkstemps "/tmp/tinydns-data-" ".tmp"
    liftIO $ hClose h1
    liftIO $ hClose h2
    let tmp1 = parseAbsFile_ tmpfn1
        tmp2 = parseAbsFile_ tmpfn2
    return (tmp1,tmp2)

  mkIO ([fmt|write: %T|] tmp1) ∘ liftIO $
       Data.Text.IO.writeFile (toFilePath tmp1) intxt
  mkProc_ @_ @() $ CmdSpec Paths.tinydns_edit ([toText tmp1,toText tmp2] ⊕ args)
  outtxt ← mkIO ([fmt|read: %T|] tmp1) ∘ liftIO $
                Data.Text.IO.readFile (toFilePath tmp1)

  -- tmp2 is auto-deleted by tinydns-data
  when (cl ≡ Clean) $
       mkIO ("clean: " ⊕ toText tmp1) ∘ liftIO $ unlink_ tmp1 -- ⪼ unlink_ tmp2

  return outtxt

addNSCmd ∷ Functor ψ ⇒ AbsFile → AbsFile → ψ FQDN → IP4 → ψ CmdSpec
addNSCmd fn tmpfn domains ip =
  let go d = tinydnsEdit [ toText fn, toText tmpfn, "add", "ns", toText d
                         , toText ip ]
   in go ⊳ domains

----------------------------------------

addHostCmd ∷ AbsFile → AbsFile → Hostname → IP4 → [CmdSpec]
addHostCmd fn tmpfn hn ip = [ CmdSpec Paths.tinydns_edit [ toText fn, toText tmpfn, "add", "host", toText hn, toText ip ] ]

mkHostCmd hn ip = do
  (fn,tmpfn) ← ask
  lift $ mkProc_ @_ @() (tinydnsEdit [ toText fn, toText tmpfn, "add", "host"
                              , toText hn, toText ip ])
mkHostCmd' t (hn,ip) = do
  tinydnsEdit' t [ "add", "host", toText hn, toText ip ]

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
      lfoowl        = [localname|foo-wl|]
      lquux         = [localname|quux|]
      lbaz          = [localname|baz|]
      lzootie1      = [localname|zootie1|]
      lzootie2      = [localname|zootie2|]
      dmn           = [fqdn|sixears.co.uk.|]
      foohost       = Host [hostname|foo.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|01:23:45:67:89:0A|])
      foowlhost     = Host [hostname|foo-wl.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|0A:89:67:45:23:01|])
      quuxhost      = Host [hostname|quux.sixears.co.uk.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|12::34:56:78:90:A0|])
      zootie1host   = Host [hostname|zootie1.sixears.co.uk.|] [ip4|192.168.1.3|]
                           "descn" ["comment"] (Just [mac|12::34:56:78:90:A0|])
      zootie2host   = Host [hostname|zootie2.sixears.co.uk.|] [ip4|192.168.1.3|]
                           "descn" ["comment"] (Just [mac|12::34:56:78:90:A0|])
      hosts         = LHostMap $ HashMap.fromList [ (lfoo,foohost)
                                                  , (lfoowl,foowlhost)
                                                  , (lquux,quuxhost)
                                                  , (lzootie1,zootie1host)
                                                  , (lzootie2,zootie2host)
                                                  ]
      dnsServers_  = [lfoo]
      mailServers_ = [lfoo]
      aliases_      = LocalnameMap $ HashMap.fromList [(lbaz,lfoo)]
   in Hosts dmn hosts dnsServers_ mailServers_ aliases_

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


  let ds = [ [fqdn|sixears.co.uk.|], [fqdn|0.168.192.in-addr.arpa.|] ]
  (tinydnsdata,es) ← __with2Temps__ (opts ⊣ clean) $ \ fn1 fn2 → system @_ @Options @_ @HostsDomainExecCreateError opts (__mkData''__ ds hs fn1 fn2)
  putStrLn tinydnsdata
  let (fn1,fn2) = ([absfile|/tmp/t1|],[absfile|/tmp/t2|])
  (t,es') ← exceptIOThrow ∘ flip runReaderT (opts ⊣ clean) $ doProcIO @_ @_ @_ @_ @HostsDomainExecCreateError opts (__mkData'''__ ds hs fn1 fn2)
  forM_ (toTexts es) $ putStrLn ∘ ("!ERROR: " ⊕)
  putStrLn t
  forM_ (toTexts es') $ putStrLn ∘ ("!RRORE: " ⊕)

----------------------------------------

runProc ∷ (MonadIO μ, AsCreateProcError ε, AsExecError ε, MonadError ε μ) ⇒
          CmdSpec → μ ()
runProc c = runProcIO (defRunProcOpts & verboseL ⊢ 1) $ mkProc_ c

__runProc__ ∷ MonadIO μ ⇒ CmdSpec → μ ()
__runProc__ = exceptIOThrow ∘ runProc @_ @ExecCreateError

----------------------------------------

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

(⧐) ∷ Functor φ ⇒ (β → γ) → (α → φ β) → α → φ γ
(⧐) = (<$$>)



system' ∷ Options → ProcIO' ExecCreateError (ExceptT ExecCreateError IO) () → IO ()
system' = system @_ @Options @_ @ExecCreateError @_ @()

mkAliasCmd ∷ (AsExecError ε, AsCreateProcError ε) ⇒
             AbsFile → AbsFile → Host → Hostname → ProcIO ε η ()
mkAliasCmd fn1 fn2 h a = mkProc_ $ addAliasCmd fn1 fn2 h a

mkAliasCmd' d t (l,h) = do
  name ← lift (l <..> d)
  tinydnsEdit' t [ "add", "alias", toText name, toText $ h ⊣ ipv4 ]

mkMxCmd ∷ (AsExecError ε, AsCreateProcError ε) ⇒
          AbsFile → AbsFile → Host → ProcIO ε η ()
mkMxCmd fn1 fn2 h = mkProc_ $ addMxCmd fn1 fn2 h

mkMxCmd' hs t h = do
  mx ← lift $ lookupHost hs h
  tinydnsEdit' t [ "add", "mx", toText (mx ⊣ hname), toText (mx ⊣ ipv4) ]

__mkData''__ ∷ (AsCreateProcError ε, AsExecError ε, AsHostsError ε, AsDomainError ε, MonadError ε μ, MonadIO μ) ⇒ [FQDN] → Hosts → AbsFile → AbsFile → ProcIO' ε μ (Text,ErrTs)
__mkData''__ domains hs fn1 fn2 = do
  es ← __mkData'__ domains hs fn1 fn2
  tinydnsdata ← lift ∘ liftIO $ Data.Text.IO.readFile (toString fn1)
  return (tinydnsdata, es)


----------------------------------------

-- given two hostnames; if one is the other+"-wl", then return the base
-- name - else return the first name, and an error
checkWL' ∷ Hostname → Hostname → (ErrTs,Hostname)
checkWL' h1 h2 =
  let (l1 :| d1) = h1 ⊣ dLabels
      (l2 :| d2) = h2 ⊣ dLabels
      errNm = [fmt|names are not "x" vs. "x-wl": '%T' vs. '%T'|] h1 h2
      errDm = [fmt|different domains: '%T' vs. '%T'|] h1 h2
   in if d1 ≡ d2
      then if toText l1 ≡ toText l2 ⊕ "-wl"
           then (ф,h2)
           else if toText l2 ≡ toText l1 ⊕ "-wl"
                then (ф,h1)
                else (errT errNm,h1)
      else (errT errDm,h1)


{- | Check that ip4, {hostnames} is actually pair of hostnames where one
     is the other + "-wl"; return the base name; or else add an error.
     The IP is passed just for the errmsg
 -}
checkWL ∷ IP4 → NonEmptyHashSet Hostname → (ErrTs, Hostname)
checkWL i hh = let errTooMany l = [fmt|too many hosts for IP %T (%L)|] i l
                in case toNEList hh of
                     h  :| []     → (ф,h)
                     h1 :| [h2]   → checkWL' h1 h2
                     lh@(h1 :| _) → (errT (errTooMany lh), h1)

{- | Check that the map ip4 -> hostnames has only pairs of hostnames where one
     is the other + "-wl"; return the base name in each case (and errors for
     ip->{many hostnames} that don't fit that rule).
 -}
filterWL ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
         → (Map.Map IP4 Hostname, ErrTs)
filterWL = let accumulator es i hh = let (es',h) = checkWL i hh in (es'⊕es, h)
            in swap ∘ mapAccumWithKey accumulator ф

{- | From a HostsDB, find all the "valid" Hostname → IP4 mappings,
     ignoring hosts called α-wl that share an IP with α; and additionally
     return errors for (other) duplicates and missing IPs, etc.
 -}
hostIPs ∷ Hosts → ([(Hostname,IP4)],ErrTs)
hostIPs hs =
  let dupIPHosts ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
      hostsByIP  ∷ Map.Map IP4 Hostname
      (dupIPHosts, hostsByIP) = fromListWithDups $ swap ⊳ hostIPv4s hs

      (filteredDups,es) = filterWL dupIPHosts
      hostList = swap ⊳ ю [ Map.toList hostsByIP
                           , Map.toList filteredDups ]
   in (hostList, es)

----------------------------------------

__mkData'__ ∷ (AsCreateProcError ε, AsExecError ε, AsHostsError ε, AsDomainError ε, MonadIO μ, MonadError ε μ) ⇒ [FQDN] → Hosts → AbsFile → AbsFile → ProcIO' ε μ ErrTs
__mkData'__ domains hs fn1 fn2 = do
  ips ← lift $ mapM (hostIPv4 hs) (hs ⊣ dnsServers)

  let (hostList,es) = hostIPs hs

--  forM_ ips $ \ ip → mkNSCmd (fn1,fn2) domains ip
  forM_ ips $ \ ip → flip runReaderT (fn1,fn2) $ mkNSCmd domains ip
  tinydnsdata ← liftIOT $ Data.Text.IO.readFile (toString fn1)

  -- mapM_ (mkProc_ @_ @()) $ ю (uncurry (addHostCmd fn1 fn2) ⊳ sortOn snd hostList)
--  foldM mkHostCmd' tinydnsdata (sortOn snd hostList)
--  liftIOT $ Data.Text.IO.writeFile (toString fn1) tinydnsdata

  let d = hs ⊣ domain
  as ← lift (HashMap.toList ⊳ aliasHosts hs)
  forM_ as (\ (l,h) → lift (l <..> d) ≫ \ a -> mkAliasCmd fn1 fn2 h a)

  forM_ (hs ⊣ mailServers) (\ a → lift (lookupHost hs a) ≫ mkMxCmd fn1 fn2)
  return es

--------------------

__mkData'''__ domains hs fn1 fn2 = do
  ips ← lift $ mapM (hostIPv4 hs) (hs ⊣ dnsServers)

  let (hostList,es) = hostIPs hs

--  forM_ ips $ \ ip → mkNSCmd (fn1,fn2) domains ip
--  forM_ ips $ \ ip → flip runReaderT (fn1,fn2) $ mkNSCmd domains ip

--  tinydnsdata ← liftIOT $ Data.Text.IO.readFile (toString fn1)
  tinydnsdata ← foldM (\ t i → mkNSCmd' t domains i) "" ips
   
  let d = hs ⊣ domain
  as ← lift (HashMap.toList ⊳ aliasHosts hs)

  tinydnsdata'  ← foldM mkHostCmd' tinydnsdata (sortOn snd hostList)
  tinydnsdata'' ← foldM (mkAliasCmd' d) tinydnsdata' as
  tinydnsdata''' ← foldM (mkMxCmd' hs) tinydnsdata'' (hs ⊣ mailServers)
--  liftIOT $ Data.Text.IO.writeFile (toString fn1) tinydnsdata'''

  return (tinydnsdata''',es)

----------------------------------------

__withTemps__ ∷ MonadIO μ ⇒ ((AbsFile,Handle) → (AbsFile,Handle) → IO α) → μ α
__withTemps__ io = let pfx1 = Just [pc|tinydns-data-|]
                       pfx2 = Just [pc|tinydns-data-.tmp|]
                    in splitMError (with2TempFiles' pfx1 pfx2 io) ≫ \ case
                       Left e → die (ExitFailure 4) e
                       Right r → return r

__with2Temps__ ∷ MonadIO μ ⇒ Clean → (AbsFile → AbsFile → IO α) → μ α
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
  liftIO $ when (cl ≡ Clean) $ unlink_ tmp1 -- ⪼ unlink_ tmp2 -- tmp2 is auto-deleted
                                                        -- by tinydns-data
  return r

liftIOT ∷ (MonadTrans κ, MonadIO μ) ⇒ IO α → κ μ α
liftIOT = lift ∘ liftIO

-- that's all, folks! ----------------------------------------------------------
