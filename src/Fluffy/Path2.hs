module Fluffy.Path2
  ()
where

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- path --------------------------------

import Path  ( Path, toFilePath )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

instance Printable (Path β τ) where
  print = P.string ∘ toFilePath

-- that's all, folks! ----------------------------------------------------------
