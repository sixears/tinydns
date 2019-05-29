{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module DomainNames.UQDN
  ( UQDN, parseUQDN, parseUQDN' )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Either    ( either )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import GHC.Generics   ( Generic )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ) )

-- fluffy ------------------------------

import Fluffy.Either   ( __right )
import Fluffy.Functor  ( (⊳) )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Iso  ( iso )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- text --------------------------------

import Data.Text  ( Text, unsnoc )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Domain             ( Domain( domainHead, prepend )
                                      , DomainLabels
                                      , IsDomainLabels( domainLabels )
                                      , parseDomainLabels'
                                      )
import DomainNames.Error.DomainError  ( DomainError( DomainEmptyErr ) )
import DomainNames.Error.UQDNError    ( AsUQDNError
                                      , UQDNError( UQDNFullyQualifiedErr )
                                      , throwAsUQDNError
                                      )

--------------------------------------------------------------------------------

newtype UQDN = UQDN { unUQDN ∷ DomainLabels }
  deriving (Eq,Generic,Hashable,Show)

instance IsDomainLabels UQDN where
  domainLabels = iso unUQDN UQDN

instance Domain UQDN where
  prepend d f = UQDN ⊳ (prepend d (f ⊣ domainLabels))
  domainHead (UQDN dls) = domainHead dls

instance Printable UQDN where
  print (UQDN dls) = P.text $ toText dls

instance Interpret UQDN where
  autoWith iopts = __parseUQDN' ⊳ autoWith iopts

parseUQDN ∷ (Printable ρ, AsUQDNError ε, MonadError ε η) ⇒ ρ → η UQDN
parseUQDN (toText → t) =
  case unsnoc t of
    Nothing →
      throwAsUQDNError DomainEmptyErr
    Just (_,'.') →
      throwAsUQDNError $ UQDNFullyQualifiedErr t
    Just _ →
      either throwAsUQDNError (return ∘ UQDN) (parseDomainLabels' t)

parseUQDN' ∷ (Printable ρ, MonadError UQDNError η) ⇒ ρ → η UQDN
parseUQDN' = parseUQDN

__parseUQDN ∷ Printable ρ ⇒ ρ → UQDN
__parseUQDN = __right ∘ parseUQDN'

__parseUQDN' ∷ Text → UQDN
__parseUQDN' = __parseUQDN

instance FromJSON UQDN where
  parseJSON (String t) = either (fail ∘ toString) return $ parseUQDN' t
  parseJSON invalid    = typeMismatch "UQDN" invalid

-- that's all, folks! ----------------------------------------------------------
