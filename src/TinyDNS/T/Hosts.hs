{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

module TinyDNS.T.Hosts
  ( tests )
where

import Debug.Trace  ( trace, traceShow )
import Prelude  ( (+) )

-- base --------------------------------

import Control.Monad    ( return )
import Data.Either      ( Either( Left, Right ) )
import Data.Eq          ( Eq )
import Data.Foldable    ( Foldable, toList )
import Data.Function    ( ($), flip )
import Data.Functor     ( fmap )
import Data.List        ( zip )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.String      ( String )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )
import Text.Show        ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- domainnames -------------------------

import DomainNames.FQDN      ( fqdn )
import DomainNames.Hostname  ( hostname, localname )

-- fluffy ------------------------------

import Fluffy.ErrTs       ( ErrTs, toTexts )
import Fluffy.Foldable    ( length )
import Fluffy.HasIndex    ( (!!) )
import Fluffy.IP4         ( ip4 )
import Fluffy.MACAddress  ( mac )
import Fluffy.MonadError  ( splitMError )
import Fluffy.Nat         ( One )
import Fluffy.Natural     ( nats )
import Fluffy.Path        ( AbsDir, AbsFile, RelFile
                          , extension, getCwd_, parseFile' )
import Fluffy.Tasty       ( (≟), assertListEq, assertListEqR, assertRight
                          , runTestsP_ )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( HostsDomainExecCreateIOError )
import HostsDB.Host              ( Host( Host ) )
import HostsDB.Hosts             ( Domains( Domains ), HasHosts( hosts )
                                 , Hosts( Hosts ) )
import HostsDB.LHostMap          ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap      ( LocalnameMap( LocalnameMap ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫), (⪼) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Reader  ( runReaderT )

-- path --------------------------------

import Path  ( File, Path, (</>), absfile, toFilePath )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRun, HasDryRunLevel( dryRunLevel )
                                  , dryRunOff, dryRunOn, dryRunP )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel ), Verbose
                                  , verboseOn, verboseP )
import ProcLib.Process            ( doProcIO )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, HasCallStack
                         , assertBool, assertFailure, testCase )

-- text --------------------------------

import Data.Text  ( Text, lines )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Hosts              ( mkData )
import TinyDNS.Types.Clean        ( HasClean( clean ), Clean( Clean, NoClean ) )
import TinyDNS.Types.TinyDNSData  ( TinyDNSData )

--------------------------------------------------------------------------------

data TestMode = TestMode | NoTestMode

data Options = Options { _dryRun   ∷ DryRun
                       , _verbose  ∷ Verbose
--                       , _input    ∷ AbsFile
--                       , _clean    ∷ Clean
--                       , _testMode ∷ TestMode
                       }
{- | just for ghci testing -}
instance Default Options where
  def =  Options dryRunOff verboseOn
--                 [absfile|/home/martyn/rc/nixos/hostcfg/sixears-hosts.dhall|]
--                 Clean TestMode

dryRun ∷ Lens' Options DryRun
dryRun = lens _dryRun (\ o d → o { _dryRun = d })

instance HasDryRunLevel One Options where
  dryRunLevel = dryRun

verbose ∷ Lens' Options Verbose
verbose = lens _verbose (\ o v → o { _verbose = v })

instance HasVerboseLevel One Options where
  verboseLevel = verbose

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

myTests ∷ Show ε ⇒ Either ε (TinyDNSData,ErrTs) → TestTree
myTests ei =
  let expect  = [ ".sixears.co.uk:192.168.1.2:a:259200"
                , ".0.168.192.in-addr.arpa:192.168.1.2:a:259200"
                , "=foo-wl.sixears.co.uk:192.168.1.2:86400"
                , "=zootie1.sixears.co.uk:192.168.1.3:86400"
                , "+baz.sixears.co.uk:192.168.1.2:86400"
                , "@foo.sixears.co.uk:192.168.1.2:a::86400"
                ]
      experrs = [   "names are not \"x\" vs. \"x-wl\": "
                  ⊕ "'zootie1.sixears.co.uk.' vs. 'zootie2.sixears.co.uk.'"
                ,   "too many hosts for IP 192.168.1.2 "
                  ⊕ "(foo-wl.sixears.co.uk.,quux.sixears.co.uk.,"
                  ⊕ "foo.sixears.co.uk.)"
                ]

  in case ei of
       Left e → testCase "mkData" $ assertFailure (show e)
       Right (t,es) → testGroup "mkData"
                                (assertListEq "tinydnsdata" expect (lines $ toText t) ⊕ assertListEq "errors" (toTexts es) experrs)

mkDataHosts ∷ Hosts → IO (Either HostsDomainExecCreateIOError (TinyDNSData, ErrTs))
mkDataHosts hs = splitMError $ flip runReaderT (RuntimeContext Clean hs) $
                      doProcIO @_ @Options @_ @_ @HostsDomainExecCreateIOError
                               def mkData

tests ∷ Show ε ⇒ Either ε (TinyDNSData,ErrTs) → TestTree
tests hs = testGroup "TinyDNS.Hosts" [ myTests hs ]

_test ∷ IO ()
_test = do
  hs ← mkDataHosts testHosts
  defaultMain (tests hs)

_tests ∷ String → IO ()
_tests p = do
  hs ← mkDataHosts testHosts
  runTestsP_ (tests hs) p


-- that's all, folks! ----------------------------------------------------------
