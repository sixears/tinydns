{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- export warnT, new exitWith
module Fluffy.MonadIO2
  ( exitWith, exitWith', warn, warnS, warnShow, warnT, write, writeOut )
where

-- base --------------------------------

import qualified  System.Exit

import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import Data.String             ( String )
import Data.Word               ( Word8 )
import System.Environment      ( getProgName )
import System.IO               ( Handle, stderr, stdout )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad  ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( hPutStr, hPutStrLn )

-- unix --------------------------------

import System.Posix.Process  ( exitImmediately )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error       ( AsIOError )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.MonadIO        ( ToExitCode( toExitCode ) )

--------------------------------------------------------------------------------

-- | putStrLn a msg to stderr, using Printable to make any necessary conversion
warn ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
warn = liftIO ∘ hPutStrLn stderr ∘ toText

-- | putStrLn a msg to stderr, specialized to Text
warnT ∷ (MonadIO μ) ⇒ Text → μ ()
warnT = warn

-- | putStrLn a msg to stderr, specialized to String
warnS ∷ (MonadIO μ) ⇒ String → μ ()
warnS = warn

-- | putStrLn a msg to stderr, specialized to String
warnShow ∷ (MonadIO μ, Show τ) ⇒ τ → μ ()
warnShow = warn ∘ show

----------------------------------------

exitWith ∷ (MonadIO m, ToExitCode e) ⇒ e → m ()
exitWith x = liftIO $ do
  getProgName ≫ \ case
    "<interactive>"→ System.Exit.exitWith (toExitCode x)
    _              → exitImmediately (toExitCode x)

exitWith' ∷ MonadIO μ ⇒ Word8 → μ ()
exitWith' = exitWith

{- | write some text, with no newline, to a `Handle` -}
write ∷ (Printable τ, AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
        Handle → τ → μ ()
write h = asIOError ∘ hPutStr h ∘ toText

{- | write some text, with no newline, to `stdout` -}
writeOut ∷ (Printable τ, AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ τ → μ ()
writeOut = write stdout

-- that's all, folks! ----------------------------------------------------------
