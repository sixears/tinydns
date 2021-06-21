{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

module TinyDNS.T.Hosts
  ( testHosts, tests )
where

-- base --------------------------------

import Data.Either      ( Either( Left, Right ) )
import Data.Function    ( ($) )
import Data.Maybe       ( Maybe( Just ) )
import Data.String      ( String )
import System.Exit      ( ExitCode )
import System.IO        ( IO )
import Text.Show        ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- domainnames -------------------------

import DomainNames.FQDN      ( fqdn )
import DomainNames.Hostname  ( hostname, localname )

-- hostsdb -----------------------------

import HostsDB.Host          ( Host( Host ) )
import HostsDB.Hosts         ( Domains( Domains ), Hosts( Hosts ) )
import HostsDB.LHostMap      ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap  ( LocalnameMap( LocalnameMap ) )

-- ip4 ---------------------------------

import IP4  ( ip4 )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- mac-address --------------------------

import MACAddress  ( mac )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Text    ( 𝕋 )

-- natural -----------------------------

import Natural  ( One )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRun, HasDryRunLevel( dryRunLevel )
                                  , dryRunOff )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel ), Verbose
                                  , verboseOff )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertFailure, testCase )

-- tasty-plus --------------------------

import TastyPlus       ( assertListEq, runTestsP, runTestTree )

-- text --------------------------------

import Data.Text  ( lines )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Error.MkTinyDNSError  ( MkTinyDNSError )
import TinyDNS.Hosts                 ( mkDataHosts )
import TinyDNS.Types.Clean           ( Clean( Clean ) )
import TinyDNS.Types.TinyDNSData     ( TinyDNSData )

--------------------------------------------------------------------------------

data Options = Options { _dryRun   ∷ DryRun
                       , _verbose  ∷ Verbose
                       }
{- | just for ghci testing -}
instance Default Options where
  def =  Options dryRunOff verboseOff

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
      dmns          = Domains [fqdn|my.domain.|]
                              [fqdn|0.168.192.in-addr.arpa.|]
      foohost       = Host [hostname|foo.my.domain.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|01:23:45:67:89:0A|])
      foowlhost     = Host [hostname|foo-wl.my.domain.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|0A:89:67:45:23:01|])
      quuxhost      = Host [hostname|quux.my.domain.|] [ip4|192.168.1.2|]
                           "descn" ["comment"] (Just [mac|12::34:56:78:90:A0|])
      zootie1host   = Host [hostname|zootie1.my.domain.|] [ip4|192.168.1.3|]
                           "descn" ["comment"] (Just [mac|12::34:56:78:90:A0|])
      zootie2host   = Host [hostname|zootie2.my.domain.|] [ip4|192.168.1.3|]
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

myTests ∷ Show ε ⇒ Either ε (TinyDNSData,[𝕋]) → TestTree
myTests ei =
  let expect  = [ ".my.domain:192.168.1.2:a:259200"
                , ".0.168.192.in-addr.arpa:192.168.1.2:a:259200"
                , "=foo-wl.my.domain:192.168.1.2:86400"
                , "=zootie1.my.domain:192.168.1.3:86400"
                , "+baz.my.domain:192.168.1.2:86400"
                , "@foo.my.domain:192.168.1.2:a::86400"
                ]
      experrs = [   "names are not \"x\" vs. \"x-wl\": "
                  ⊕ "'zootie1.my.domain.' vs. 'zootie2.my.domain.'"
                ,   "too many hosts for IP 192.168.1.2 "
                  ⊕ "(foo-wl.my.domain.,quux.my.domain.,"
                  ⊕ "foo.my.domain.)"
                ]

  in case ei of
       Left e → testCase "mkData" $ assertFailure (show e)
       Right (t,es) → testGroup "mkData"
                                ([ assertListEq "tinydnsdata"
                                                   (lines $ toText t)
                                                   expect
                                 , assertListEq "errors" es experrs
                                 ]
                                )

{-
mkDataHosts ∷ Hosts
            → IO (Either HostsDomainExecCreateIOError (TinyDNSData, ErrTs))
mkDataHosts hs = ѥ $ flip runReaderT (RuntimeContext Clean hs)
                             $ doProcIO @_ @Options def mkData
-}

tests ∷ Show ε ⇒ Either ε (TinyDNSData,[𝕋]) → TestTree
tests hs = testGroup "TinyDNS.Hosts" [ myTests hs ]

_test ∷ IO ExitCode
_test = do
  hs ← ѥ $ mkDataHosts @MkTinyDNSError @_ @_ @Options Clean testHosts def
  runTestTree (tests hs)

_tests ∷ String → IO ExitCode
_tests p = do
  hs ← ѥ $ mkDataHosts @MkTinyDNSError @_ @_ @Options Clean testHosts def
  runTestsP (tests hs) p

-- that's all, folks! ----------------------------------------------------------
