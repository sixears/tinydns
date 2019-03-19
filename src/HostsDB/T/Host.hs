{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.T.Host
  ( tests )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- domainnames -------------------------

import DomainNames.Hostname  ( hostname, __parseHostname', parseHostname' )

-- data-textual ------------------------

import Data.Textual  ( toString, toText ) 

-- fluffy ------------------------------

import Fluffy.Tasty   ( assertRight )
import Fluffy.Tasty2  ( (≟) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertEqual, testCase )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Host"
    [ testCase "hostname parse" $
        assertRight (\ h → assertEqual (toString h) "foo.bar." (toText h))
                    (parseHostname' ("foo.bar." ∷ Text))
    , testCase "hostname qq" $ [hostname|foo.bar.|] ≟ __parseHostname' "foo.bar."
    ]

-- that's all, folks! ----------------------------------------------------------
