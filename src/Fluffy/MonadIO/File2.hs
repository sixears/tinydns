{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module Fluffy.MonadIO.File2
  ( readFileBinary     , readFileBinary'     , __readFileBinary__
  , readFileUTF8       , readFileUTF8'       , __readFileUTF8__
  , readFileUTF8Lenient, readFileUTF8Lenient', __readFileUTF8Lenient__
  , writeFileUtf8      , writeFileUtf8'      , __writeFileUtf8__
  , writeFileBinary    , writeFileBinary'    , __writeFileBinary__
  )
where

-- base --------------------------------

import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import System.IO               ( IOMode( ReadMode, WriteMode )
                               , hSetEncoding, utf8, withFile )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- path --------------------------------

import Path  ( File, Path, toFilePath )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( Text )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor2  ( (⩺) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.IO.Error       ( AsIOError, IOError )
import Fluffy.MonadError.IO  ( asIOError )

--------------------------------------------------------------------------------

-- cribbed shamelessly from RIO.Prelude.IO

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Path β File → μ Text
readFileUTF8 = asIOError ∘ __readFileUTF8__

readFileUTF8' ∷ (MonadError IOError μ, MonadIO μ) ⇒ Path β File → μ Text
readFileUTF8' = readFileUTF8

__readFileUTF8__ ∷ MonadIO μ ⇒ Path β File → μ Text
__readFileUTF8__ (toFilePath → fp) = liftIO $ withFile fp ReadMode $ \ h → do
  hSetEncoding h utf8
  TextIO.hGetContents h

--------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                      Path β File → μ Text
readFileUTF8Lenient = asIOError ∘ __readFileUTF8Lenient__

readFileUTF8Lenient' ∷ (MonadError IOError μ, MonadIO μ) ⇒
                       Path β File → μ Text
readFileUTF8Lenient' = readFileUTF8Lenient

__readFileUTF8Lenient__ ∷ MonadIO μ ⇒ Path β File → μ Text
__readFileUTF8Lenient__ = decodeUtf8With lenientDecode ⩺ __readFileBinary__

----------------------------------------

{- | Write a file in UTF8 encoding using OS-specific line-ending handling. -}
writeFileUtf8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                Path β File → Text → μ ()
writeFileUtf8 fn = asIOError ∘ __writeFileUtf8__ fn

writeFileUtf8' ∷ (MonadError IOError μ, MonadIO μ) ⇒ Path β File → Text → μ ()
writeFileUtf8' = writeFileUtf8

__writeFileUtf8__ ∷ MonadIO μ ⇒ Path β File → Text → μ ()
__writeFileUtf8__ (toFilePath → fp) text =
  liftIO $ withFile fp WriteMode $ \h → do
    hSetEncoding h utf8
    TextIO.hPutStr h text

----------------------------------------

-- | Same as 'BS.readFile', but generalized to 'MonadIO'
readFileBinary ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                 Path β File → μ ByteString
readFileBinary = asIOError ∘ __readFileBinary__

readFileBinary' ∷ (MonadError IOError μ, MonadIO μ) ⇒ Path β File → μ ByteString
readFileBinary' = readFileBinary

__readFileBinary__ ∷ MonadIO μ ⇒ Path β File → μ ByteString
__readFileBinary__ = liftIO ∘ BS.readFile ∘ toFilePath

----------------------------------------

-- | Same as 'BS.writeFile', but generalized to 'MonadIO'
writeFileBinary ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                  Path β File → ByteString → μ ()
writeFileBinary fn = asIOError ∘ __writeFileBinary__ fn

writeFileBinary' ∷ (MonadError IOError μ, MonadIO μ) ⇒
                   Path β File → ByteString → μ ()
writeFileBinary' = writeFileBinary

__writeFileBinary__ ∷ MonadIO μ ⇒ Path β File → ByteString → μ ()
__writeFileBinary__ (toFilePath → fp) = liftIO ∘ BS.writeFile fp


-- that's all, folks! ----------------------------------------------------------
