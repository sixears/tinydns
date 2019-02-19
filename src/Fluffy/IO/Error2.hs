{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Fluffy.IO.Error2
  ( )
where

-- base --------------------------------

import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error  ( IOError )

--------------------------------------------------------------------------------

instance Printable IOError where
  print = P.string ∘ show

-- that's all, folks! ----------------------------------------------------------
