{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.IP4
  ( IP4, ip4 )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Applicative  ( empty, pure )
import Control.Monad        ( fail, return )
import Data.Eq              ( Eq )
import Data.Function        ( ($) )
import Data.Functor         ( fmap )
import Data.Ord             ( Ord )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Parsed( Parsed, Malformed ), Printable( print )
                     , parseText, toText )

-- dhall -------------------------------

import qualified  Dhall.Core  as  DC

import Dhall  ( Interpret( autoWith ), Type( Type, expected, extract ) )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⋪), (⊵) )
import Fluffy.Functor2     ( (⊳) )
import Fluffy.Parsec2      ( Parsecable( parser ), __parsecN )
import Fluffy.Quasi        ( mkQuasiQuoterExp )

-- network-ip --------------------------

import qualified  Network.IP.Addr  as  IPAddr

-- parsec ------------------------------

import Text.Parsec.Char  ( char )

-- template-haskell --------------------

import Language.Haskell.TH        ( appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

--------------------------------------------------------------------------------

newtype IP4 = IP4 IPAddr.IP4
  deriving (Eq, Ord, Show)

instance FromJSON IP4 where
  parseJSON (String t) = case parseText t of
                           Parsed    i4  → return $ IP4 i4
                           Malformed _ e → fail (e ⊕ " (" ⊕ unpack t ⊕ ")")
  parseJSON invalid    = typeMismatch "IP4" invalid

instance Printable IP4 where
  print (IP4 ipv4) = P.text (toText ipv4)

------------------------------------------------------------

instance Parsecable IP4 where
  parser = fmap IP4 $ IPAddr.ip4FromOctets ⊳ parser ⋪ char '.'
                                           ⊵ parser ⋪ char '.'
                                           ⊵ parser ⋪ char '.'
                                           ⊵ parser


__ip4 ∷ Text → IP4
__ip4 = __parsecN

{-| quasi-quoter for ipv4 addresses -}
ip4 ∷ QuasiQuoter
ip4 = mkQuasiQuoterExp "IP4" ( \ t → appE (varE '__ip4) (litE $ stringL t) )

instance Interpret IP4 where
  autoWith _ = Type {..}
               where extract (DC.TextLit (DC.Chunks [] t)) = pure $ __parsecN t
                     extract _                             = empty
                     expected = DC.Text

-- that's all, folks! ----------------------------------------------------------
