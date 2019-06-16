{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.ErrTs
  ( ErrTs, errT, toTexts )
where

-- base --------------------------------

import Data.Monoid     ( Monoid( mempty ) )
import Data.Semigroup  ( Semigroup( (<>) ) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

newtype ErrTs = ErrTs [Text]

instance Semigroup ErrTs where
  (ErrTs es) <> (ErrTs es') = ErrTs (es ⊕ es')

instance Monoid ErrTs where
  mempty = ErrTs []

toTexts ∷ ErrTs → [Text]
toTexts (ErrTs ts) = ts

errT ∷ Text → ErrTs
errT t = ErrTs [t]

-- that's all, folks! ----------------------------------------------------------
