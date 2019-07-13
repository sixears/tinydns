{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.T.Hostname
  ( tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.String    ( String )
import System.IO      ( IO )

-- data-textual ------------------------

import Data.Textual  ( toString, toText ) 

-- fluffy ------------------------------

import Fluffy.Tasty  ( (≟), assertRight, runTestsP_ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertEqual, testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Hostname  ( hostname, __parseHostname', parseHostname' )

--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Hostname"
    [ testCase "parseHostname" $
        assertRight (\ h → assertEqual (toString h) "foo.bar." (toText h))
                    (parseHostname' ("foo.bar." ∷ Text))
    , testCase "hostname" $ [hostname|foo.bar.|] ≟ __parseHostname' "foo.bar."
    ]

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = do
  _ ← runTestsP_ tests p
  return ()

-- that's all, folks! ----------------------------------------------------------
