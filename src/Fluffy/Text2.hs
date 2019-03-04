{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Text2
  ( splitOn )
where

-- base --------------------------------

import Data.List.NonEmpty  ( NonEmpty, fromList )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- text --------------------------------

import qualified  Data.Text

import Data.Text  ( Text )

--------------------------------------------------------------------------------

{-| `Data.Text.splitOn` always returns a non-empty list - even an empty string
    returns a list (`[""]`); we encode that here
-}
splitOn ∷ Text → Text → NonEmpty Text
splitOn s = fromList ∘ Data.Text.splitOn s

-- that's all, folks! ----------------------------------------------------------
