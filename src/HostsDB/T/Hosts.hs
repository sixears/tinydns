{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.T.Hosts
  ( tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( auto )

-- domainnames -------------------------

import DomainNames.FQDN      ( fqdn )
import DomainNames.Hostname  ( hostname, localname )

-- fluffy ------------------------------

import Fluffy.Functor     ( (⊳) )
import Fluffy.IP4         ( ip4 )
import Fluffy.MACAddress  ( macAddress )
import Fluffy.Tasty       ( assertListEqIO, runTestsP_, withResource' )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- mono-traversable --------------------

import Data.MonoTraversable  ( otoList )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Monad  ( (≫) )
import Data.MoreUnicode.Tasty  ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- text --------------------------------

import Data.Text  ( Text, unlines )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host          ( Host( Host ) )
import HostsDB.Hosts         ( Domains( Domains ), Hosts( Hosts )
                             , aliases, dnsServers, lhostmap, inAddr
                             , mailServers, subDomain
                             )
import HostsDB.LHostMap      ( LHostMap( LHostMap ) )
import HostsDB.LocalnameMap  ( LocalnameMap( LocalnameMap ) )

--------------------------------------------------------------------------------

hostsTestHosts ∷ Hosts
hostsTestHosts =
  let chrome = Host [hostname|chrome.sixears.co.uk.|] [ip4|192.168.0.6|]
                    "study desktop server" []
                    (Just [macAddress|fc:aa:14:87:cc:a2|])
      winxp  = Host [hostname|winxp.sixears.co.uk.|] [ip4|192.168.0.87|]
                    "VirtualBox on Chrome" []
                    (Just [macAddress|08:00:27:23:08:43|])
      cargo  = Host [hostname|cargo.sixears.co.uk.|] [ip4|192.168.0.9|]
                    "DVR" [] (Just [macAddress|e0:cb:4e:ba:be:60|])
      expHostMap = LHostMap $ HashMap.fromList [ ([localname|winxp|] , winxp)
                                               , ([localname|chrome|], chrome)
                                               , ([localname|cargo|] , cargo)
                                               ]
   in Hosts (Domains [fqdn|sixears.co.uk.|] [fqdn|0.168.192.in-addr.arpa.|])
            expHostMap 
            [ [localname|cargo|], [localname|chrome|] ]
            [ [localname|cargo|] ]
            (LocalnameMap $ HashMap.fromList
               [ ([localname|mailhost|], [localname|cargo|])
               , ([localname|www|]     , [localname|chrome|])
               , ([localname|cvs|]     , [localname|chrome|])
               ]
            )
  
hostsTestText ∷ Text
hostsTestText =
  unlines [ "{ domains = { sub_domain = \"sixears.co.uk.\""
          , "            , in_addr    = \"0.168.192.in-addr.arpa.\" }"
          , ", aliases = [ { from = \"mailhost\", to = \"cargo\"}"
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

dhallTests' ∷ IO Hosts → TestTree
dhallTests' hs =
  testGroup "dhallTests" $ [ testCase "sub-domain" $
                               hs ≫ \hs' →   hostsTestHosts ⊣ subDomain
                                           ≟ hs' ⊣ subDomain
                           , testCase "in-addr" $
                               hs ≫ \hs' →   hostsTestHosts ⊣ inAddr
                                           ≟ hs' ⊣ inAddr
                           ]
                         ⊕ assertListEqIO "aliases"
                                              (otoList $ hostsTestHosts ⊣ aliases)
                                              (otoList ∘ view aliases ⊳ hs)
                         ⊕ assertListEqIO "dnsServers"
                                              (hostsTestHosts ⊣ dnsServers)
                                              (view dnsServers ⊳ hs)
                         ⊕ assertListEqIO "mailServers"
                                              (hostsTestHosts ⊣ mailServers)
                                              (view mailServers ⊳ hs)
                         ⊕ assertListEqIO "hosts"
                                           (otoList $ hostsTestHosts ⊣ lhostmap)
                                           (otoList ∘ view lhostmap ⊳ hs)
dhallTests ∷ TestTree
dhallTests = withResource' (D.input auto hostsTestText) dhallTests'

----------------------------------------

tests ∷ TestTree
tests = testGroup "Hosts" [ dhallTests ]

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = do
  _ ← runTestsP_ tests p
  return ()

-- that's all, folks! ----------------------------------------------------------
