{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Indexable
  ( Indexable( index ), (!), (!!) )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Data.Function    ( flip )
import Data.Maybe       ( Maybe )
import Numeric.Natural  ( Natural )

-- safe --------------------------------

import Safe  ( atMay )

--------------------------------------------------------------------------------

class Indexable χ where
  type Indexer χ
  type Elem    χ
  index ∷ Indexer χ → χ → Maybe (Elem χ)

infixl 9 !
(!) ∷ Indexable χ ⇒ Indexer χ → χ → Maybe (Elem χ)
i ! xs = index i xs

infixl 9 !!
(!!) ∷ Indexable χ ⇒ χ → Indexer χ → Maybe (Elem χ)
(!!) = flip (!)


instance Indexable [α] where
  type Indexer [α] = Natural
  type Elem    [α] = α
  index i xs = atMay xs (fromIntegral i)

-- that's all, folks! ----------------------------------------------------------
