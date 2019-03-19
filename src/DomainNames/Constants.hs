{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Constants
  ( maxDomainLength, maxLabelLength )
where

-- base --------------------------------

import Numeric.Natural  ( Natural )

--------------------------------------------------------------------------------

maxLabelLength ∷ Natural
maxLabelLength = 63
    
maxDomainLength ∷ Natural
maxDomainLength = 253

-- that's all, folks! ----------------------------------------------------------
