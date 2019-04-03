{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Equalish
  ( Equalish( (≏) ) )
where

-- base --------------------------------

import Data.List.NonEmpty  ( NonEmpty )
import Data.Maybe          ( Maybe )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

infix 4 ≏

class Equalish α where
  {- | Like `==`; but gives a list of texts specifying where the differences
       are, if any.  Original defined for use in testing record-style data types
   -}
  (≏) ∷ α → α → Maybe (NonEmpty Text)

-- that's all, folks! ----------------------------------------------------------
