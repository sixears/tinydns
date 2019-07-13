{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.T.Host
  ( tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import System.IO      ( IO )

-- domainnames -------------------------

import DomainNames.Hostname  ( hostname )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( auto )

-- fluffy ------------------------------

import Fluffy.Equalish    ( (≏) )
import Fluffy.IP4         ( ip4 )
import Fluffy.MACAddress  ( macAddress )
import Fluffy.Tasty       ( (≟), ioTests, runTestsP_, withResource' )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- text --------------------------------

import Data.Text  ( Text, unlines )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host  ( Host( Host ) )

--------------------------------------------------------------------------------

chromeTxt ∷ Text
chromeTxt = unlines [ " { fqdn = \"chrome.sixears.co.uk.\""
                    , " , ipv4 = \"192.168.0.6\""
                    , " , desc = \"study desktop server\""
                    , " , mac= [ \"fc:aa:14:87:cc:a2\" ] : Optional Text"
                    , " , comments = [] : List Text"
                    , " }"
                    ]

chrome ∷ Host
chrome = Host [hostname|chrome.sixears.co.uk.|]
              [ip4|192.168.0.6|]
              "study desktop server"
              []
              (Just [macAddress|fc:aa:14:87:cc:a2|])

chromeTxtTest ∷ IO Host → TestTree
chromeTxtTest =  ioTests "chromeTxt" [ ("chrome", \ h → Nothing ≟ chrome ≏ h) ]

tests ∷ TestTree
tests =
  testGroup "Host" [ withResource' (D.input auto chromeTxt) chromeTxtTest
                   ]

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = do
  _ ← runTestsP_ tests p
  return ()

-- that's all, folks! ----------------------------------------------------------
