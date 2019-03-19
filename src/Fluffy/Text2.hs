{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Text2
  ( bracket, parenthesize, qquote, quote,  splitOn, surround )
where

-- base --------------------------------

import Data.List.NonEmpty  ( NonEmpty, fromList )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- text --------------------------------

import qualified  Data.Text

import Data.Text  ( Text )

--------------------------------------------------------------------------------

{-| `Data.Text.splitOn` always returns a non-empty list - even an empty string
    returns a list (`[""]`); we encode that here
-}
splitOn ∷ Text → Text → NonEmpty Text
splitOn s = fromList ∘ Data.Text.splitOn s

parenthesize ∷ Text → Text
parenthesize t = "(" ⊕ t ⊕ ")"

bracket ∷ Text → Text
bracket t = "[" ⊕ t ⊕ "]"

surround ∷ Text → Text → Text
surround q t = q ⊕ t ⊕ q

quote ∷ Text -> Text
quote = surround "'"

qquote ∷ Text -> Text
qquote = surround "\""

-- that's all, folks! ----------------------------------------------------------
