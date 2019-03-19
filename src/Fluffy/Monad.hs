{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Monad
  ( (⪼), (≪), (≫), (⋘), (⋙), returnPair )
where

-- base --------------------------------

import Control.Monad  ( Monad, (>>), (>>=), (=<<), return )
import Data.Functor   ( Functor, fmap )

--------------------------------------------------------------------------------

-- | unicode alias for `(>>)`
infixl 1 ⪼
(⪼) ∷ Monad η ⇒ η α → η β → η β
(⪼) = (>>)

-- | unicode alias for `(>>=)`
infixl 1 ≫
(≫) ∷ Monad η ⇒ η α → (α → η β) → η β
(≫) = (>>=)

-- | unicode alias for `(=<<)`
infixr 1 ≪
(≪) ∷ Monad η ⇒ (α → η β) → η α → η β
(≪) = (=<<)

{- | a bit like `(=<<)` / `(⋘)`, but allows the rhs to be a function itself
     for point-free styling
-}
(⋘) ∷ (Monad η, Functor ψ) ⇒ (α → η β) → ψ (η α) → ψ (η β)
x ⋘ y = fmap (x ≪) y

{- | a bit like `(>>=)` / `(⋙)`, but allows the rhs to be a function itself
     for point-free styling
-}
(⋙) ∷ (Monad η, Functor ψ) ⇒ ψ (η α) → (α → η β) → ψ (η β)
x ⋙ y = fmap (≫ y) x

returnPair ∷ Monad η ⇒ (η α, η β) → η (α,β)
returnPair (a,b) = a ≫ \ a' → b ≫ \ b' → return (a',b')

-- that's all, folks! ----------------------------------------------------------
