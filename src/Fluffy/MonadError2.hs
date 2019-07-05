{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.MonadError2
  ( ѥ )
where

-- base --------------------------------

import Control.Monad  ( Monad )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadError  ( splitMError )

--------------------------------------------------------------------------------

ѥ ∷ (MonadError ε η, Monad μ) ⇒ ExceptT ε μ α → μ (η α)
ѥ = splitMError

-- that's all, folks! ----------------------------------------------------------
