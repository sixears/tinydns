{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.T.MACAddress
  ( tests )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Either     ( Either( Right ) )
import Data.Function   ( ($) )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import System.IO       ( IO )
import Text.Show       ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( fromText, toString, toText, toUtf8 )

-- dhall -------------------------------

import Dhall  ( input, auto )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-quickcheck --------------------

import Test.Tasty.QuickCheck  ( Property, testProperty )

-- text --------------------------------

import Data.Text  ( Text )

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml

import Data.Yaml  ( decodeEither', encode, prettyPrintParseException )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Functor2       ( (⊳) )
import Fluffy.MACAddress     ( MACAddress( MACAddress ), mac )
import Fluffy.Monad          ( (≫) )
import Fluffy.Parsec2        ( parsec )
import Fluffy.Parsec.Error2  ( ParseError )
import Fluffy.Tasty          ( runTestsP_ )
import Fluffy.Tasty2         ( (≟), (≣), assertIsLeft )
import Fluffy.Text2          ( qquote )

--------------------------------------------------------------------------------

testMAC1 ∷ MACAddress
testMAC1 = MACAddress 17 34 51 68 85 102

testMAC1T ∷ Text
testMAC1T = "11-22-33-44-55-66"

testMAC1T_ ∷ Text
testMAC1T_ = "[ 17, 34, 51, 68, 85, 102 ]"

testMAC1T__ ∷ Text
testMAC1T__ = "[ \"11\", '22', '33', \"44\", '55', 102 ]"

testMAC2 ∷ MACAddress
testMAC2 = MACAddress 175 0 222 173 190 239

testMAC2T ∷ Text
testMAC2T = "af-00-de-ad-be-ef"

testMAC2T' ∷ Text
testMAC2T' = "af-00-DE:AD:BE:EF"

testMAC2T_ ∷ Text
testMAC2T_ = "[ 175, \"00\", \"DE\", \"AD\", \"be\", \"ef\" ]"

badMAC1T ∷ Text
badMAC1T = "11-22-33-44-55-g0"

badMAC2T ∷ Text
badMAC2T = "0-22-33-44-55-d0"

badMAC3T ∷ Text
badMAC3T = "010-22-33-44-55-a0"

badMAC4T ∷ Text
badMAC4T = "01+22-33-44-55-f0"

----------------------------------------

printableTests ∷ TestTree
printableTests =
  testGroup "Printable" [ testCase "testMAC1" $ testMAC1T ≟ toText testMAC1
                        , testCase "testMAC2" $ testMAC2T ≟ toText testMAC2
                        ]

propInvertibleTextual ∷ MACAddress → Property
propInvertibleTextual m = fromText (toText m) ≣ Just m

textualTests ∷ TestTree
textualTests =
  let fromText' ∷ Text → Maybe MACAddress
      fromText' = fromText
  in testGroup "Textual"
                [ testCase "testMAC1T"  $ Just testMAC1 ≟ fromText  testMAC1T
                , testCase "testMAC2T"  $ Just testMAC2 ≟ fromText  testMAC2T
                , testCase "testMAC2T'" $ Just testMAC2 ≟ fromText  testMAC2T'
                , testCase "badMAC1"    $ Nothing ≟ fromText' badMAC1T
                , testCase "badMAC2"    $ Nothing ≟ fromText' badMAC2T
                , testCase "badMAC3"    $ Nothing ≟ fromText' badMAC3T
                , testCase "badMAC4"    $ Nothing ≟ fromText' badMAC4T
                , testProperty "fromText ∘ toText" propInvertibleTextual
                ]

propInvertibleJSON ∷ MACAddress → Property
propInvertibleJSON m = first show (decodeEither' $ encode m) ≣ Right m

fromYamlTests ∷ TestTree
fromYamlTests =
  let fromYAML = first prettyPrintParseException ⊳ Yaml.decodeEither' ∘ toUtf8
   in testGroup "fromYaml"
                [ testCase "testMAC1T"   $ Right testMAC1 ≟ fromYAML testMAC1T
                , testCase "testMAC1T_"  $ Right testMAC1 ≟ fromYAML testMAC1T_
                , testCase "testMAC1T__" $ Right testMAC1 ≟ fromYAML testMAC1T__
                , testCase "testMAC2T"   $ Right testMAC2 ≟ fromYAML testMAC2T
                , testCase "testMAC2T'"  $ Right testMAC2 ≟ fromYAML testMAC2T'
                , testCase "testMAC2T_"  $ Right testMAC2 ≟ fromYAML testMAC2T_
                , testCase "badMAC1"     $ assertIsLeft (fromYAML badMAC1T)
                , testCase "badMAC2"     $ assertIsLeft (fromYAML badMAC2T)
                , testCase "badMAC3"     $ assertIsLeft (fromYAML badMAC3T)
                , testCase "badMAC4"     $ assertIsLeft (fromYAML badMAC4T)
                , testProperty "fromJSON ∘ toJSON" propInvertibleJSON
                ]

parsecableTests ∷ TestTree
parsecableTests =
  let parsec_ ∷ Text → Text → Either ParseError MACAddress
      parsec_ = parsec
      testR name expect txt =
        testCase (toString name) $ Right expect ≟ parsec_ name txt
      testL name txt =
        testCase (toString name) $ assertIsLeft (parsec_ name txt)
   in testGroup "Parsecable" [ testR "testMAC1T" testMAC1 testMAC1T
                             , testR "testMAC2T" testMAC2 testMAC2T
                             , testR "testMAC2T'" testMAC2 testMAC2T'
                             , testL "badMAC1" badMAC1T
                             , testL "badMAC2" badMAC2T
                             , testL "badMAC3" badMAC3T
                             , testL "badMAC4" badMAC4T
                             ]

quasiTests ∷ TestTree
quasiTests =
  testGroup "QuasiQuoting"
            [ testCase "testMAC1"  $ testMAC1 ≟ [mac|11-22-33-44-55-66|]
            , testCase "testMAC2"  $ testMAC2 ≟ [mac|af-00-de-ad-be-ef|]
            , testCase "testMAC2'" $ testMAC2 ≟ [mac|af-00-DE:AD:BE:EF|]
            ]

dhallTests ∷ TestTree
dhallTests = let ttest name expect txt =
                   testCase name $ input auto (qquote txt) ≫ (expect ≟)
              in testGroup "Dhall" [ ttest "testMAC1" testMAC1 testMAC1T
                                   , ttest "testMAC2" testMAC2 testMAC2T
                                   , ttest "testMAC2" testMAC2 testMAC2T'
                                   ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "MACAddress" [ printableTests, textualTests, fromYamlTests
                               , parsecableTests, quasiTests, dhallTests ]

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = runTestsP_ tests p


-- that's all, folks! ----------------------------------------------------------
