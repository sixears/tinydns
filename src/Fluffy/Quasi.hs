{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Quasi
  ( mkQuasiQuoterExp )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Function  ( ($) )
import Data.String    ( String )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ )
import Language.Haskell.TH.Quote  ( QuasiQuoter( QuasiQuoter, quoteDec
                                               , quoteExp, quotePat, quoteType )
                                  )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

mkQuasiQuoterExp ∷ Text → (String → ExpQ) → QuasiQuoter
mkQuasiQuoterExp n f = let notImpl u = error $ [fmt|%t %t  not implemented|] n u
                        in QuasiQuoter { quoteDec  = notImpl "quoteDec"
                                       , quoteType = notImpl "quoteType"
                                       , quotePat  = notImpl "quotePat"
                                       , quoteExp = f
                                       }

-- that's all, folks! ----------------------------------------------------------
