{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Lens2
  ( (⊣), (⊢), (⋕), (⩼) )
where

-- base --------------------------------

import Data.Maybe   ( Maybe )
import Data.Monoid  ( First )

-- lens --------------------------------

import Control.Lens.Fold    ( (^?) )
import Control.Lens.Getter  ( Getting, (^.) )
import Control.Lens.Review  ( AReview, re )
import Control.Lens.Setter  ( ASetter, (.~) )

--------------------------------------------------------------------------------

infixl 8 ⊣
(⊣) ∷ δ → Getting α δ α → α
(⊣) = (^.)

(⋕) :: AReview t s -> s -> t
x ⋕ y = y ^. re x

infixr 4 ⊢
(⊢) ∷ ASetter σ τ α β → β → σ → τ
(⊢) = (.~)

infixl 8 ⩼
(⩼) ∷ σ -> Getting (First α) σ α -> Maybe α
(⩼) = (^?)

-- that's all, folks! ----------------------------------------------------------
