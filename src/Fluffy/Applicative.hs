{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Applicative
  ( (⊵), (⊴), (∤), (⋪), (⋫) )
where

-- base --------------------------------

import Control.Applicative  ( Alternative, Applicative
                            , (<*>), (<**>), (<|>), (<*), (*>) )

--------------------------------------------------------------------------------

infixl 1 ⊵
(⊵) ∷ Applicative φ ⇒ φ (α → β) → φ α → φ β
(⊵) = (<*>)

infixr 1 ⊴
(⊴) ∷ Applicative φ ⇒ φ α → φ (α → β) → φ β
(⊴) = (<**>)

infixr 1 ∤
(∤) ∷ Alternative φ ⇒ φ α → φ α → φ α
(∤) = (<|>)

infixl 4 ⋪
(⋪) ∷ Applicative φ ⇒ φ α → φ β → φ α
(⋪) = (<*)

infixl 4 ⋫
(⋫) ∷ Applicative φ ⇒ φ α → φ β → φ β
(⋫) = (*>)

-- that's all, folks! ----------------------------------------------------------
