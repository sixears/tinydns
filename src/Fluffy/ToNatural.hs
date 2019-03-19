{-# LANGUAGE UnicodeSyntax #-}

module Fluffy.ToNatural
  ( ToNatural( toNatural ) )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Data.Function    ( id )
import Data.Word        ( Word8, Word16, Word32, Word64 )
import Numeric.Natural  ( Natural )

--------------------------------------------------------------------------------

class ToNatural χ where
  toNatural ∷ χ → Natural

instance ToNatural Natural where
  toNatural = id

instance ToNatural Word8 where
  toNatural = fromIntegral

instance ToNatural Word16 where
  toNatural = fromIntegral

instance ToNatural Word32 where
  toNatural = fromIntegral

instance ToNatural Word64 where
  toNatural = fromIntegral

-- that's all, folks! ----------------------------------------------------------
