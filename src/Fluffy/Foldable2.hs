{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Foldable2
  ( HasLength( length ) )
where

-- base --------------------------------

import qualified  Data.Foldable 

import Numeric.Natural     ( Natural )
import Prelude             ( fromIntegral )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- text --------------------------------

import qualified  Data.Text

import Data.Text  ( Text )

--------------------------------------------------------------------------------

class HasLength α where
  length ∷ α → Natural

instance HasLength Text where
  length = fromIntegral ∘ Data.Text.length

instance Data.Foldable.Foldable α ⇒ HasLength (α β) where
  length = fromIntegral ∘ Data.Foldable.length

-- that's all, folks! ----------------------------------------------------------
