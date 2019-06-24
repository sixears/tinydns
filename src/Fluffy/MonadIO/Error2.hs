{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.MonadIO.Error2
  ( eToIO )
where

-- base --------------------------------

import Control.Exception       ( Exception, throwIO )
import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( Either, either )
import Data.Function           ( ($) )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadError  ( splitMError )

--------------------------------------------------------------------------------

eToIO ∷ (MonadIO μ, Exception ε) ⇒ IO (Either ε α) → μ α
eToIO io = liftIO $ io ≫ either throwIO return

exceptIOThrow ∷ (Exception ε, MonadIO μ) ⇒ ExceptT ε IO α → μ α
exceptIOThrow = eToIO ∘ splitMError

-- that's all, folks! ----------------------------------------------------------
