{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.T.Dhall
  ( tests )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )
import Data.Either             ( Either( Right ) )
import Data.Function           ( ($) )
import Data.Maybe              ( isJust )
import Data.String             ( String )
import GHC.Num                 ( Integer )
import Numeric.Natural         ( Natural )
import System.IO               ( IO )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall

import Dhall  ( Interpret )

-- fluffy ------------------------------

import Fluffy.Tasty  ( assertLeft, runTestsP_ )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens      ( (⩼) )
import Data.MoreUnicode.Monad     ( (≫) )
import Data.MoreUnicode.Tasty     ( (≟) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Dhall               ( parse, parseT, tryDhall )
import Fluffy.Dhall.Error         ( AsDhallParseError( _DhallParseError )
                                  , AsDhallSomeError( _DhallSomeError )
                                  , DhallError, _DhallTypeSrcError )
import Fluffy.Dhall.ErrorOrphans  ( )
import Fluffy.MonadError2         ( ѥ )

--------------------------------------------------------------------------------

tryDhallTests ∷ TestTree
tryDhallTests =
  let tryD ∷ (MonadIO μ, MonadError DhallError η, NFData α) ⇒ IO α → μ (η α)
      tryD = ѥ ∘ tryDhall
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "tryDhall"
                [ testCase "integer success" $
                      tryD (Dhall.input Dhall.integer "+7") ≫ (Right 7 ≟)
                , testCase "natural success" $
                      tryD (Dhall.input Dhall.natural  "7") ≫ (Right 7 ≟)
                , testCase "natural ≢ integer" $
                      tryD (Dhall.input Dhall.integer  "7") ≫
                          assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      tryD (Dhall.input Dhall.natural  "+7") ≫
                          assertLeft isTypeSrcError
                , testCase "parseError" $
                      tryD (Dhall.input Dhall.natural  "[") ≫
                          assertLeft isParseError
                , testCase "import error" $
                      tryD (Dhall.input Dhall.natural  "./nonesuch") ≫
                          assertLeft isSomeError
                ]

----------------------------------------

parseTests ∷ TestTree
parseTests =
  let parse_ ∷ (NFData α, Interpret α, MonadIO μ, MonadError DhallError η) ⇒
               Text → μ (η α)
      parse_ = ѥ ∘ parse
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "parse"
                [ testCase "integer success" $
                      parse_ "+7" ≫ (Right (7 ∷ Integer) ≟)
                , testCase "natural success" $
                      parse_ "7" ≫ (Right (7 ∷ Natural) ≟)
                , testCase "natural ≢ integer" $
                      parse_ @Integer "7" ≫ assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      parse_ @Natural "+7" ≫ assertLeft isTypeSrcError
                , testCase "parseError" $
                      parse_ @Natural "[" ≫ assertLeft isParseError
                , testCase "import error" $
                      parse_ @Natural "./nonesuch" ≫ assertLeft isSomeError
                ]

----------------------------------------

parseTTests ∷ TestTree
parseTTests =
  let parseT_ ∷ (NFData α, Interpret α, MonadIO μ, MonadError DhallError η) ⇒
                Dhall.Type α → Text → μ (η α)
      parseT_ y = ѥ ∘ parseT y
      isErrorType p e = assertBool (show e) (isJust $ e ⩼ p)
      isTypeSrcError  = isErrorType _DhallTypeSrcError
      isParseError    = isErrorType _DhallParseError
      isSomeError     = isErrorType _DhallSomeError
   in testGroup "parseT"
                [ testCase "integer success" $
                      parseT_ Dhall.integer "+7" ≫ (Right 7 ≟)
                , testCase "natural success" $
                      parseT_ Dhall.natural "7" ≫ (Right 7 ≟)
                , testCase "natural ≢ integer" $
                      parseT_ Dhall.integer "7" ≫ assertLeft isTypeSrcError
                , testCase "integer ≢ natural" $
                      parseT_ Dhall.natural "+7" ≫ assertLeft isTypeSrcError
                , testCase "parseTError" $
                      parseT_ Dhall.natural "[" ≫ assertLeft isParseError
                , testCase "import error" $
                      parseT_ Dhall.natural "./nonesuch" ≫ assertLeft isSomeError
                ]

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Fluffy.Dhall" [ tryDhallTests, parseTests, parseTTests ]

_test ∷ IO ()
_test = defaultMain tests

_tests ∷ String → IO ()
_tests p = runTestsP_ tests p


-- that's all, folks! ----------------------------------------------------------
