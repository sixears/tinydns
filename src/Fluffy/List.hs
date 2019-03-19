{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.List
  ( (⋮) )
where

-- base --------------------------------

import Data.Foldable       ( Foldable, toList )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )

--------------------------------------------------------------------------------

infixr 5 ⋮
(⋮) ∷ Foldable ψ ⇒ α → ψ α → NonEmpty α
x ⋮ xs = x :| toList xs

-- that's all, folks! ----------------------------------------------------------

