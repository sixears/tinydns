{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Main
  ( doMain )
where

-- base --------------------------------

import Data.Either  ( Either( Left, Right ) )
import System.Exit  ( ExitCode( ExitFailure ) )
import System.IO    ( IO )
import Text.Show    ( Show )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (⪼) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadError2  ( ѥ )
import Fluffy.MonadIO      ( MonadIO, ToExitCode
                           , exitWith, print )

--------------------------------------------------------------------------------

exitFail ∷ MonadIO μ ⇒ μ α
exitFail = exitWith (ExitFailure 255)

doMain ∷ (Show ε, ToExitCode σ) ⇒ ExceptT ε IO σ → IO ()
doMain f = do
  m ← ѥ f
  case m of
    Left  e → print e ⪼ exitFail
    Right x → do {-
                 let c = toExitCode x
                 if c ≡ ExitSuccess
                 then return ()
                 else do p ← getProgName
                         if "<interactive>" ≡ p
                         then exitWith x
                         else exitImmediately c
                 -}
                 exitWith x

-- that's all, folks! ----------------------------------------------------------
