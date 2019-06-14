{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad       ( foldM, forM_, mapM, return )
import Data.Bifunctor      ( first )
import Data.Either         ( Either, either )
import Data.Eq             ( Eq )
import Data.Foldable       ( Foldable, toList )
import Data.Function       ( ($), (&), flip, id )
import Data.Functor        ( fmap )
import Data.List           ( sortOn )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Maybe          ( Maybe( Just ) )
import Data.Monoid         ( Monoid( mconcat, mempty ) )
import Data.Semigroup      ( Semigroup( (<>) ) )
import Data.String         ( String )
import Data.Tuple          ( snd, swap )
import System.IO           ( IO )
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

import Data.Textual  ( toText )

-- dhall -------------------------------

import Dhall  ( auto, defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- domainnames -------------------------

import DomainNames.Domain                ( IsDomainLabels( dLabels ) )
import DomainNames.Error.DomainError     ( AsDomainError )
import DomainNames.FQDN                  ( FQDN, fqdn )
import DomainNames.Hostname              ( Hostname, Localname
                                         , (<..>), hostname, localname )

-- fluffy ------------------------------

import Fluffy.Containers.NonEmptyHashSet
                             ( NonEmptyHashSet, toNEList )
import Fluffy.IO.Error       ( AsIOError )
import Fluffy.IP4            ( IP4, ip4 )
import Fluffy.MACAddress     ( mac )
import Fluffy.MapUtils       ( fromListWithDups )
import Fluffy.MonadIO        ( MonadIO, dieUsage, liftIO )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Nat            ( One )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( AbsDir, AbsFile, RelFile
                             , extension, getCwd_, parseFile' )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( AsHostsError, HostsDomainExecCreateIOError )
import HostsDB.Host              ( Host( Host ), hname, ipv4 )
import HostsDB.Hosts             ( Domains( Domains ), Hosts( Hosts )
                                 , aliasHosts, dnsServers, domains, hostIPv4
                                 , hostIPv4s, inAddr, lookupHost, mailServers
                                 , subDomain
                                 )
import HostsDB.LHostMap          ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap      ( LocalnameMap( LocalnameMap ) )

-- lens --------------------------------

import Control.Lens.Getter   ( view )
import Control.Lens.Lens     ( Lens', lens )
import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( MonadReader, asks, runReaderT )
import Control.Monad.Trans   ( lift )

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
import ProcLib.Process                ( doProcIO, mkProc_, runProcIO )
import ProcLib.Types.CmdSpec          ( CmdSpec )
import ProcLib.Types.ProcIO           ( ProcIO )
import ProcLib.Types.RunProcOpts      ( defRunProcOpts, verboseL )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, pack )
import Data.Text.IO  ( putStrLn )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import Data.Yaml  ( decodeEither' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Edit               ( tinydnsAddNS, tinydnsEdit )
import TinyDNS.Types.Clean        ( HasClean( clean ), Clean( Clean, NoClean ) )
import TinyDNS.Types.TinyDNSData  ( TinyDNSData )

--------------------------------------------------------------------------------

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

instance HasClean Options where
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

class HasHosts α where
  hosts ∷ Lens' α Hosts

testHosts ∷ Hosts
testHosts =
  let lfoo          = [localname|foo|]
      lfoowl        = [localname|foo-wl|]
      lquux         = [localname|quux|]
      lbaz          = [localname|baz|]
      lzootie1      = [localname|zootie1|]
      lzootie2      = [localname|zootie2|]
      dmns          = Domains [fqdn|sixears.co.uk.|]
                              [fqdn|0.168.192.in-addr.arpa.|]
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
      hsts          = LHostMap $ HashMap.fromList [ (lfoo,foohost)
                                                  , (lfoowl,foowlhost)
                                                  , (lquux,quuxhost)
                                                  , (lzootie1,zootie1host)
                                                  , (lzootie2,zootie2host)
                                                  ]
      dnsServers_  = [lfoo]
      mailServers_ = [lfoo]
      aliases_      = LocalnameMap $ HashMap.fromList [(lbaz,lfoo)]
   in Hosts dmns hsts dnsServers_ mailServers_ aliases_

data RuntimeContext = RuntimeContext { _clean_ ∷ Clean
                                     , _hosts_ ∷ Hosts
                                     }
  deriving (Eq, Show)

instance HasClean RuntimeContext where
  clean = lens _clean_ (\ rc cl → rc { _clean_ = cl })

instance HasHosts RuntimeContext where
  hosts = lens _hosts_ (\ rc hs → rc { _hosts_ = hs })


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


  (t,es') ← exceptIOThrow ∘ flip runReaderT (RuntimeContext (opts ⊣ clean) hs) $ doProcIO @_ @_ @_ @_ @HostsDomainExecCreateIOError opts (mkData hs)
  putStrLn (toText t)
  forM_ (toTexts es') $ putStrLn ∘ ("!ERROR: " ⊕)

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

------------------------------------------------------------

mkNSCmd ∷ (Foldable ψ, AsCreateProcError ε, AsExecError ε, AsIOError ε,
           MonadIO μ, HasClean α, MonadReader α μ) ⇒
          TinyDNSData → ψ FQDN → IP4 → ProcIO ε μ TinyDNSData
mkNSCmd intxt ds ip =
  foldM (\ t d → tinydnsAddNS d ip t) intxt ds

--------------------

mkNSCmds ∷ (AsCreateProcError ε, AsExecError ε, AsHostsError ε, AsIOError ε,
            MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
           TinyDNSData → ProcIO ε μ TinyDNSData
mkNSCmds tinydnsdata = do
  hs  ← lift (asks $ view hosts)
  ips ← lift $ mapM (hostIPv4 hs) (hs ⊣ dnsServers)
  foldM (\ t i → mkNSCmd t [hs ⊣ subDomain, hs ⊣ inAddr] i) tinydnsdata ips

----------------------------------------

tinydnsAddHost ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
                  MonadIO μ, HasClean α, MonadReader α μ) ⇒
                 Hostname → IP4 → TinyDNSData → ProcIO ε μ TinyDNSData
tinydnsAddHost hn ip =
  tinydnsEdit [ "add", "host", toText hn, toText ip ]

mkHostCmd ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
             HasClean α, MonadReader α μ, MonadIO μ) ⇒
            TinyDNSData → (Hostname, IP4) → ProcIO ε μ TinyDNSData
mkHostCmd t (hn,ip) = tinydnsAddHost hn ip t

----------------------------------------

mkHostCmds ∷ (Foldable ψ,
              AsCreateProcError ε, AsExecError ε, AsIOError ε,
              MonadIO μ, HasClean α, MonadReader α μ) ⇒
             ψ (Hostname,IP4) → TinyDNSData → ProcIO ε μ TinyDNSData
mkHostCmds hostList t = foldM mkHostCmd t (sortOn snd $ toList hostList)

----------------------------------------

tinydnsAddAlias ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
                   MonadIO μ, HasClean α, MonadReader α μ) ⇒
                  Hostname → Host → TinyDNSData → ProcIO ε μ TinyDNSData
tinydnsAddAlias name h =
  tinydnsEdit [ "add", "alias", toText name, toText $ h ⊣ ipv4 ]

mkAliasCmd ∷ (AsCreateProcError ε, AsDomainError ε, AsExecError ε, AsIOError ε,
              HasClean α, MonadReader α μ, MonadIO μ) ⇒
             Domains → TinyDNSData → (Localname,Host) → ProcIO ε μ TinyDNSData
mkAliasCmd d t (l,h) = do
  name ← lift (l <..> (d ⊣ subDomain))
  tinydnsAddAlias name h t

mkAliasCmds ∷ (AsCreateProcError ε, AsExecError ε,
               AsDomainError ε, AsHostsError ε, AsIOError ε,
               MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
              TinyDNSData → ProcIO ε μ TinyDNSData
mkAliasCmds t = do
  hs ← lift (asks $ view hosts)
  let d = hs ⊣ domains
  as ← lift (HashMap.toList ⊳ aliasHosts hs)
  foldM (mkAliasCmd d) t as

----------------------------------------

tinydnsAddMx ∷ (AsCreateProcError ε, AsExecError ε, AsIOError ε,
                MonadIO μ, HasClean α, MonadReader α μ) ⇒
               Host → TinyDNSData → ProcIO ε μ TinyDNSData
tinydnsAddMx mx =
  tinydnsEdit [ "add", "mx", toText (mx ⊣ hname), toText (mx ⊣ ipv4) ]

mkMxCmd ∷ (AsExecError ε, AsCreateProcError ε, AsHostsError ε, AsIOError ε,
           HasClean α, MonadReader α μ, MonadIO μ) ⇒
          Hosts → TinyDNSData → Localname → ProcIO ε μ TinyDNSData
mkMxCmd hs t h = do
  mx ∷ Host ← lift $ lookupHost hs h
  tinydnsAddMx mx t

mkMxCmds ∷ (AsExecError ε, AsCreateProcError ε, AsHostsError ε, AsIOError ε,
            MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒
           TinyDNSData → ProcIO ε μ TinyDNSData

mkMxCmds t = do
  hs ← lift (asks $ view hosts)
  foldM (mkMxCmd hs) t (hs ⊣ mailServers)

----------------------------------------

mkData ∷ (AsHostsError ε, AsExecError ε, AsCreateProcError ε,
          AsDomainError ε, AsIOError ε,
          MonadIO μ, HasClean α, HasHosts α, MonadReader α μ) ⇒

         Hosts → ProcIO ε μ (TinyDNSData, ErrTs)

mkData hs = do
  let (hostList,es) = hostIPs hs

  tinydnsdata ← mkNSCmds ф ≫ mkHostCmds hostList ≫ mkAliasCmds ≫ mkMxCmds

  return (tinydnsdata,es)


-- that's all, folks! ----------------------------------------------------------
