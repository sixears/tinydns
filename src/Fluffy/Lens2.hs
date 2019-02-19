{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Lens2
  ( (⊣), (⊢), (⋕) )
where

-- lens --------------------------------

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

-- that's all, folks! ----------------------------------------------------------
