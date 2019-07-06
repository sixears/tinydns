{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- !!! Cleanup Temp Files !!!
-- !!! REWRITE Fluffy.TempFile TO NOT USE LAZY IO !!!

-- base --------------------------------

import Control.Monad           ( forM_, return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Word               ( Word8 )
import System.IO               ( Handle, IO, stdout )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- fluffy ------------------------------

import Fluffy.Dhall.Error    ( AsDhallError, DhallIOError )
import Fluffy.ErrTs          ( toTexts )
import Fluffy.IO.Error       ( AsIOError )
import Fluffy.Main           ( doMain )
import Fluffy.MonadError.IO  ( asIOError )
import Fluffy.MonadIO        ( warn )
import Fluffy.MonadIO.Error  ( exceptIOThrow )
import Fluffy.Options        ( optParser )
import Fluffy.Path           ( getCwd_ )

-- hostsdb -----------------------------

import HostsDB.Hosts  ( loadFile )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text --------------------------------

import Data.Text.IO  ( hPutStr )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import TinyDNS.Hosts                        ( mkDataHosts' )
import TinyDNS.Types.MkTinyDNSData.Options  ( input, parseOptions )
import TinyDNS.Types.Clean                  ( HasClean( clean ) )

--------------------------------------------------------------------------------

write ∷ (Printable τ, AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
        Handle → τ → μ ()
write h = asIOError ∘ hPutStr h ∘ toText

writeOut ∷ (Printable τ, AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ τ → μ ()
writeOut = write stdout

__main__ ∷ (MonadIO μ, AsIOError ε, AsDhallError ε, MonadError ε μ) ⇒ μ Word8
__main__ = do
  cwd  ← getCwd_
  opts ← optParser "make tiny dns data from hosts config" (parseOptions cwd)

  hs ← loadFile (opts ⊣ input)

  (t,es) ← exceptIOThrow $ mkDataHosts' (opts ⊣ clean) hs opts
  writeOut (toText t)
  forM_ (toTexts es) $ warn ∘ ("!ERROR: " ⊕)
  return 0

__main'__  ∷ (MonadIO μ, MonadError DhallIOError μ) ⇒ μ Word8
__main'__ = __main__

-- --help pre-defined to fail 2 (special helper, parser, etc.)

main ∷ IO ()
main = doMain __main'__

-- that's all, folks! ----------------------------------------------------------
