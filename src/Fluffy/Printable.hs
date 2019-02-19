{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Printable
  ( __ERR, q )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Function  ( ($) )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

{- | throw an error with some printable -}
__ERR ∷ Printable ρ ⇒ ρ → α
__ERR s = error $ toString s

{- | surround text with (single) quotes -}
q ∷ Printable ρ ⇒ ρ -> Text
q t = "'" ⊕ toText t ⊕ "'"

-- that's all, folks! ----------------------------------------------------------
