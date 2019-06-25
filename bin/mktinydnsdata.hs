{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- !!! Cleanup Temp Files !!!
-- !!! REWRITE Fluffy.TempFile TO NOT USE LAZY IO !!!

import Prelude ( error )

-- base --------------------------------

import Control.Monad   ( forM_, return )
import Data.Either     ( either )
import Data.Function   ( ($), (&) )
import System.IO       ( IO )
import Text.Show       ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- bytestring --------------------------

import Data.ByteString  ( readFile )

-- data-textual ------------------------

import Data.Textual  ( toText )

-- dhall -------------------------------

import Dhall  ( auto, defaultInputSettings, inputWithSettings, rootDirectory
              , sourceName )

-- fluffy ------------------------------

import Fluffy.ErrTs          ( toTexts )
import Fluffy.MonadIO        ( MonadIO, dieUsage, liftIO, warn )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( extension, getCwd_ )

-- hostsdb -----------------------------

import HostsDB.Hosts  ( Hosts )

-- lens --------------------------------

import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )

-- path --------------------------------

import Path  ( File, Path, toFilePath )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text.IO  ( putStr )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

-- yaml --------------------------------

import Data.Yaml  ( decodeEither' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Hosts                        ( mkDataHosts' )
import TinyDNS.Types.MkTinyDNSData.Options  ( input, parseOptions )
import TinyDNS.Types.Clean                  ( HasClean( clean ) )

--------------------------------------------------------------------------------

__loadFileYaml__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileYaml__ fn = liftIO $
  decodeEither' ⊳ readFile (toFilePath fn) ≫ either (error ∘ show) return

__loadFileDhall__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileDhall__ fn =
  let baseDir       = rootDirectory ⊢ toFilePath fn ⊣ directory
      inputSettings = defaultInputSettings & sourceName ⊢ toFilePath fn
                                           & baseDir
      inputDhall    = inputWithSettings inputSettings auto
   in liftIO $ Data.Text.IO.readFile (toFilePath fn) ≫ inputDhall

----------------------------------------

main ∷ IO ()
main = do
  cwd  ← getCwd_
  opts ← optParser "make tiny dns data from hosts config" (parseOptions cwd)

  let infn = opts ⊣ input
      ext  = infn ⊣ extension
  hs ← let badExt = [fmtT|file ext not recognized: '%t'|] ext
        in case ext of
             ".yaml"  → __loadFileYaml__  (opts ⊣ input)
             ".dhall" → __loadFileDhall__ (opts ⊣ input)
             _        → dieUsage badExt


  (t,es') ← exceptIOThrow $ mkDataHosts' (opts ⊣ clean) hs opts
  putStr (toText t)
  forM_ (toTexts es') $ warn ∘ ("!ERROR: " ⊕)

-- that's all, folks! ----------------------------------------------------------
