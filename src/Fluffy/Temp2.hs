{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Temp2
  ( mktempf, mktemps )
where

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Bifunctor          ( first )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe, maybe )
import System.IO               ( Handle )

-- base-unicode-functions --------------

import Data.Function.Unicode  ( (∘) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- path --------------------------------

import Path  ( Path, Abs, toFilePath )

-- unix --------------------------------

import System.Posix.Temp  ( mkstemps )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error       ( AsIOError )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.Path           ( AbsFile, RelFile, parseAbsFile_ )

--------------------------------------------------------------------------------

hClose ∷  (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

mktemps ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
          Path Abs τ → Maybe RelFile → μ (AbsFile, Handle)
mktemps pfx sfx =
  asIOError ∘ fmap (first parseAbsFile_) $ mkstemps (toFilePath pfx) (maybe "" toFilePath sfx)

mktempf ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
          Path Abs τ → Maybe RelFile → μ AbsFile
mktempf pfx sfx = do
  (fn,h) ← mktemps pfx sfx
  hClose h
  return fn

-- that's all, folks! ----------------------------------------------------------
