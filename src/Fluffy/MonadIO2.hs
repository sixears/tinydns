{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.MonadIO2
  ( die, dieUsage )
where

-- base --------------------------------

import Data.Word      ( Word8 )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Monad    ( (⪼) )
import Fluffy.MonadIO  ( MonadIO, ToExitCode, exitWith, warn )

--------------------------------------------------------------------------------

die :: (MonadIO μ, ToExitCode δ, Printable ρ) => δ -> ρ -> μ α
die ex msg = warn (toText msg) ⪼ exitWith ex

dieUsage :: (MonadIO μ, Printable ρ) => ρ -> μ α
dieUsage = die (2 :: Word8)


-- that's all, folks! ----------------------------------------------------------
