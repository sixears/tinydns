{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.Natural
  ( ℕ, nats )
where

-- base --------------------------------

import Numeric.Natural  ( Natural )

--------------------------------------------------------------------------------

type ℕ = Natural

nats ∷ [ℕ]
nats = [0∷ℕ ..]

-- that's all, folks! ----------------------------------------------------------
