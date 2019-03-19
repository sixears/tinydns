{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Either
  ( __right, leftFail, toLeft, toRight )
where

-- base --------------------------------

import Control.Monad  ( Monad, fail, return )
import Data.Either    ( Either, either )
import Data.Function  ( flip, id )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Printable ( __ERR )

--------------------------------------------------------------------------------

toLeft ∷ (β → α) → Either α β → α
toLeft = either id

toRight ∷ (α → β) → Either α β → β
toRight = flip either id

__right ∷ Printable ε ⇒ Either ε β → β
__right = toRight __ERR

leftFail ∷ (Printable ε, Monad η) ⇒ Either ε α → η α
leftFail = either (fail ∘ toString) return

-- that's all, folks! ----------------------------------------------------------
