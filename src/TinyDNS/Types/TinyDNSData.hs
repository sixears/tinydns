{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module TinyDNS.Types.TinyDNSData
  ( TinyDNSData( TinyDNSData ) )
where

-- base --------------------------------

import Data.Eq         ( Eq )
import Data.Monoid     ( Monoid )
import Data.Semigroup  ( Semigroup )
import Text.Show       ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

newtype TinyDNSData = TinyDNSData Text
  deriving (Eq, Monoid, Semigroup, Show)

instance Printable TinyDNSData where
  print (TinyDNSData t) = P.text t

-- that's all, folks! ----------------------------------------------------------
