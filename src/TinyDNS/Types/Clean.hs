{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TinyDNS.Types.Clean
  ( Clean( Clean, NoClean ), HasClean( clean ) )
where

-- base --------------------------------

import Data.Eq    ( Eq )
import Text.Show  ( Show )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data Clean    = Clean | NoClean
  deriving (Eq, Show)

class HasClean α where
  clean ∷ Lens' α Clean

-- that's all, folks! ----------------------------------------------------------
