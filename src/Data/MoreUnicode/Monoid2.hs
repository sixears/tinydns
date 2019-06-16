{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.MoreUnicode.Monoid2
  ( ф, ю )
where

-- base --------------------------------

import Data.Foldable  ( Foldable, toList )
import Data.Monoid    ( Monoid, mconcat, mempty )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

--------------------------------------------------------------------------------

ф ∷ Monoid α ⇒ α
ф = mempty
ю ∷ (Foldable φ, Monoid α) ⇒ φ α → α
ю = mconcat ∘ toList

-- that's all, folks! ----------------------------------------------------------
