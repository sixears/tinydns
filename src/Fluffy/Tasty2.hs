{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module Fluffy.Tasty2
  ( (≟), assertListEq, assertListEqIO, assertListEqIO', ioTests )
where

-- base --------------------------------

import Control.Monad    ( return )
import Data.Eq          ( Eq )
import Data.Foldable    ( Foldable, toList )
import Data.Function    ( ($) )
import Data.Functor     ( fmap )
import Data.List        ( zip )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import System.IO        ( IO )
import Text.Show        ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode      ( (≡) )
import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup )

-- test-tasty --------------------------

import Test.Tasty.HUnit  ( Assertion, (@=?), assertBool, testCase )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Foldable   ( length )
import Fluffy.Functor2   ( (⊳) )
import Fluffy.Indexable  ( (!!) )
import Fluffy.Monad      ( (≫) )
import Fluffy.Natural    ( nats )

--------------------------------------------------------------------------------

{- | Pronounced 'test', this tests for equality; it's `(@=?)`; note that puts
     the 'got' or 'actual' value as the last argument, to allow for easier
     partial application.
 -}
infix 1 ≟
(≟) ∷ (Eq α, Show α) ⇒ α → α → Assertion
(≟) = (@=?)
  
{- | Construct a test group, wherein each test is passed a value that has been
     pre-initialized in some IO.  Note that the IO is not run for each test, it
     is run no more than once (and that, of course, only if the tests are run).
 -}
ioTests ∷ TestName → [(TestName, α → Assertion)] → IO α → TestTree
ioTests name tests ioa =
  testGroup name $ (\ (tname,t) → testCase tname $ ioa ≫ t) ⊳ tests

assertEq' ∷ Eq t ⇒ (t → Text) → t → t → Assertion
assertEq' toT expect got =
  assertBool ([fmt|expected: %t\nbut got : %t|] (toT expect) (toT got)) $
             expect ≡ got

{- | Compare two lists for equality, with itemized testing.  We take the inputs
     as IO to allow for, well, IO.
 -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → [TestTree]
assertListEqIO' toT name (toList → expect) (fmap toList → got) =
  let lCheck e g = assertBool ([fmt|length %d did not match expected %d|] g e)
                              (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) = testCase ([fmt|%T: %w|] name i)
                                  (got ≫ \ g → assertEq' toT' (Just e) (g !! i))
      toT' Nothing  = "Nothing"
      toT' (Just a) = "Just " ⊕ toT a

   in testCase ([fmt|%T: count|] name) (got ≫ lengthCheck expect)
    : (assertItem ⊳ zip nats expect)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α) ⇒
                Text → ψ α → IO (φ α) → [TestTree]
assertListEqIO = assertListEqIO' toText

assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ) ⇒
               Text -> ψ α -> φ α -> [TestTree]
assertListEq name expect got = assertListEqIO name expect (return got)

-- that's all, folks! ----------------------------------------------------------
