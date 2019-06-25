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
import Data.Maybe      ( Maybe( Just ) )
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
import Fluffy.Path           ( AbsDir, extension, getCwd_ )
import Fluffy.Path2          ( readAbsFile )

-- hostsdb -----------------------------

import HostsDB.Hosts  ( Hosts )

-- lens --------------------------------

import System.FilePath.Lens  ( directory )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢) )

-- optparse-applicative ----------------

import qualified  Options.Applicative.Types  as  OptParse

import Options.Applicative.Builder  ( argument, flag, help, long, metavar )

-- path --------------------------------

import Path  ( File, Path, toFilePath )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( dryRunP )
import ProcLib.CommonOpt.Verbose  ( verboseP )

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

import TinyDNS.Hosts          ( mkDataHosts' )
import TinyDNS.Types.MkTinyDNSData.Options
                              ( Options( Options ), input )
import TinyDNS.Types.Clean    ( HasClean( clean )
                              , Clean( Clean, NoClean ) )

--------------------------------------------------------------------------------

parseOptions ∷ AbsDir → OptParse.Parser Options
parseOptions cwd =
  let helpText = "don't delete intermediate files"
   in Options ⊳ dryRunP
              ⊵ verboseP
              ⊵ argument (readAbsFile $ Just cwd) (metavar "HOSTS.dhall")
              ⊵ flag Clean NoClean (long "no-clean" ⊕ help helpText)

-- | Perform some IO within a temporary directory freshly created by `mkTempDir`.
--   Cleans away the created directory when IO is complete.

__loadFileYaml__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileYaml__ fn = liftIO $
  decodeEither' ⊳ readFile (toFilePath fn) ≫ either (error ∘ show) return

__loadFileDhall__ ∷ MonadIO μ ⇒ Path β File → μ Hosts
__loadFileDhall__ fn = liftIO $
--  Data.Text.IO.readFile (toFilePath fn) ≫ D.inputFrom (toFilePath fn) auto
    Data.Text.IO.readFile (toFilePath fn) ≫ inputWithSettings (defaultInputSettings & sourceName ⊢ toFilePath fn & rootDirectory ⊢ toFilePath fn ⊣ directory) auto

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


--  (t,es') ← exceptIOThrow ∘ flip runReaderT (RuntimeContext (opts ⊣ clean) hs) $ doProcIO @_ @_ @_ @_ @HostsDomainExecCreateIOError opts mkData
  (t,es') ← exceptIOThrow $ mkDataHosts' (opts ⊣ clean) hs opts
  putStr (toText t)
  forM_ (toTexts es') $ warn ∘ ("!ERROR: " ⊕)

-- that's all, folks! ----------------------------------------------------------
