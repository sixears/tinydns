{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Either
  ( __right, toLeft, toRight )
where

-- base --------------------------------

import Data.Either    ( Either, either )
import Data.Function  ( flip, id )

-- data-textual ------------------------

import Data.Textual  ( Printable )

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

-- that's all, folks! ----------------------------------------------------------
