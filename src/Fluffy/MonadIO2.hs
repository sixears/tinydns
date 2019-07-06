{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- export warnT
module Fluffy.MonadIO2
  ( warnT )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadIO  ( warn )

--------------------------------------------------------------------------------

-- | putStrLn a msg to stderr, using Printable to make any necessary conversion
warnT ∷ (MonadIO μ, Printable t) ⇒ t → μ ()
warnT = warn ∘ toText

-- that's all, folks! ----------------------------------------------------------
