{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Path2
  ( readAbsFile )
where

-- base --------------------------------

import Control.Exception  ( SomeException( SomeException ) )
import Control.Monad      ( return )
import Data.Bifunctor     ( first )
import Data.Either        ( Either( Left, Right ) )
import Data.Function      ( ($) )
import Data.Functor       ( fmap )
import Data.Maybe         ( Maybe )
import Data.String        ( String )
import Text.Show          ( show )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( ReadM, eitherReader )

-- path --------------------------------

import Path  ( PathException( InvalidAbsFile ), (</>), toFilePath )

-- text --------------------------------

import Data.Text  ( pack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.MonadError  ( fromMaybe )
import Fluffy.Path        ( AbsDir, AbsFile, PathError, RelFile, parseFile' )
import Fluffy.Path.Error  ( pathError' )

--------------------------------------------------------------------------------

{- | Read (as an argument or option, in the `Options.Applicative` sense) a file,
     which if not explicitly absolute will be taken to be relative to a given
     dir (e.g., the cwd)
 -}
readAbsFile ∷ Maybe AbsDir → ReadM AbsFile
readAbsFile cwd =
  eitherReader go
  where noRelE r = pathError' "cannot parse relative file" r
                              (SomeException $ InvalidAbsFile (toFilePath r))
        eToAbs ∷ MonadError PathError μ ⇒
                 Either AbsFile RelFile → μ AbsFile
        eToAbs = \ case
                   Left  a → return a
                   Right r → fromMaybe (noRelE r) (fmap (</> r) cwd)
        go ∷ String → Either String AbsFile
        go s = first show $ parseFile' (pack s) ≫ eToAbs

-- that's all, folks! ----------------------------------------------------------
