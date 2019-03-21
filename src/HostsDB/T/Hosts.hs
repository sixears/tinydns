{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.T.Hosts
  ( tests )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Nothing ) )
import System.IO      ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( auto )

-- domainnames -------------------------

import DomainNames.Hostname  ( hostname, localname )

-- fluffy ------------------------------

import Fluffy.Functor2  ( (⊳) )
import Fluffy.IP4       ( ip4 )
import Fluffy.Tasty2    ( assertListEqIO, withResource' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( otoList )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- text --------------------------------

import Data.Text  ( Text, unlines )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host          ( Host( Host ) )
import HostsDB.Hosts         ( Hosts( Hosts ), hosts )
import HostsDB.HostMap       ( HostMap( HostMap ) )
import HostsDB.LocalHostMap  ( LocalHostMap( LocalHostMap ) )

--------------------------------------------------------------------------------

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

dhallTests' ∷ IO Hosts → TestTree
dhallTests' hs =
  testGroup "dhallTests" $ assertListEqIO "hosts"
                                              (otoList $ hosts hostsTestHosts)
                                              (otoList ∘ hosts ⊳ hs)
dhallTests ∷ TestTree
dhallTests = withResource' (D.input auto hostsTestText) dhallTests'


tests ∷ TestTree
tests = testGroup "Hosts" [ dhallTests ]


-- that's all, folks! ----------------------------------------------------------
