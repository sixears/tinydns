{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Fluffy.Dhall
  ( parse, parse', parseT, parseT'
  , parseFile, parseFile', parseFileT, parseFileT', parseFileTS, parseFileTS'
  , tryDhall, tryDhall' )
where

-- base --------------------------------

import Control.Exception       ( SomeException )
import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Either             ( either )
import Data.Function           ( (&) )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- dhall -------------------------------

import qualified  Dhall

import Dhall  ( InputSettings, Interpret
              , defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- enclosed-exceptions -----------------

import Control.Exception.Enclosed  ( tryAnyDeep )

-- lens --------------------------------

import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- path --------------------------------

import Path  ( File, Path, toFilePath )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Dhall.Error    ( AsDhallError, DhallError, DhallIOError
                             , mkDhallError )
import Fluffy.IO.Error       ( AsIOError )
import Fluffy.MonadError     ( mapMError )
import Fluffy.MonadIO.File2  ( readFileUTF8 )

--------------------------------------------------------------------------------

tryAnyME ∷ (MonadIO μ, MonadError ε η, NFData α) ⇒
           (SomeException → ε) → IO α → μ (η α)
tryAnyME f = mapMError f ⩺ liftIO ∘ (either throwError return ⩺ tryAnyDeep)

tryDhall ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α) ⇒ IO α → μ α
--tryDhall io = fmap (first mkDhallError) $ tryAnyDeep io
tryDhall = join ∘ tryAnyME mkDhallError

tryDhall' ∷ (MonadIO μ, MonadError DhallError μ, NFData α) ⇒ IO α → μ α
tryDhall' = tryDhall

----------------------------------------

parseT ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α) ⇒
         Dhall.Type α → Text → μ α
parseT t = tryDhall ∘ Dhall.input t

parseT' ∷ (MonadIO μ, MonadError DhallError μ, NFData α) ⇒
          Dhall.Type α → Text → μ α
parseT' = parseT

parse ∷ (AsDhallError ε, MonadError ε μ, MonadIO μ, NFData α, Interpret α) ⇒
        Text → μ α
parse = parseT Dhall.auto

parse' ∷ (MonadIO μ, MonadError DhallError μ, NFData α, Interpret α) ⇒
         Text → μ α
parse' = parse

----------------------------------------

parseFile ∷ (AsDhallError ε, AsIOError ε, MonadError ε μ, MonadIO μ,
             NFData α, Interpret α) ⇒
            Path β File → μ α
parseFile = parseFileT Dhall.auto

--------------------

parseFile' ∷ (MonadError DhallIOError μ, MonadIO μ, NFData α, Interpret α) ⇒
             Path β File → μ α
parseFile' = parseFile

----------------------------------------

parseFileT ∷ (AsDhallError ε, AsIOError ε, MonadError ε μ,
              MonadIO μ, NFData α) ⇒
             Dhall.Type α → Path β File → μ α
parseFileT t fn =
  let baseDir       = toFilePath fn ⊣ directory
      inputSettings = defaultInputSettings & sourceName    ⊢ toFilePath fn
                                           & rootDirectory ⊢ baseDir
   in parseFileTS inputSettings t fn

--------------------

parseFileT' ∷ (MonadError DhallIOError μ, MonadIO μ, NFData α) ⇒
             Dhall.Type α → Path β File → μ α

parseFileT' = parseFileT

----------------------------------------

{- | parse a Dhall file, specifying the expected type, and input settings -}
parseFileTS ∷ (AsDhallError ε, AsIOError ε, MonadError ε μ,
              MonadIO μ, NFData α) ⇒
             InputSettings → Dhall.Type α → Path β File → μ α
parseFileTS s t fn = readFileUTF8 fn ≫ tryDhall ∘ inputWithSettings s t

--------------------

parseFileTS' ∷ (MonadError DhallIOError μ, MonadIO μ, NFData α) ⇒
               InputSettings → Dhall.Type α → Path β File → μ α
parseFileTS' = parseFileTS


-- that's all, folks! ----------------------------------------------------------
