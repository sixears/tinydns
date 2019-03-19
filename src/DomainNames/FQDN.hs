{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module DomainNames.FQDN
  ( FQDN, fqdn, parseFQDN, parseFQDN' )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, return )
import Data.Either    ( either )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ) )

-- fluffy ------------------------------

import Fluffy.Either    ( __right )
import Fluffy.Functor2  ( (⊳) )
import Fluffy.Quasi     ( mkQuasiQuoterExp )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unsnoc )

-- yaml --------------------------------

import Data.Yaml  ( FromJSON( parseJSON ), Value( String ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Domain             ( Domain( domainHead, domainLabels
                                              , prepend )
                                      , DomainLabels, parseDomainLabels'
                                      )
import DomainNames.Error.DomainError  ( DomainError( DomainEmptyErr ) )
import DomainNames.Error.FQDNError    ( AsFQDNError
                                      , FQDNError( FQDNNotFullyQualifiedErr )
                                      , throwAsFQDNError
                                      )

--------------------------------------------------------------------------------

newtype FQDN = FQDN DomainLabels
  deriving (Eq, Hashable, Show)

instance Domain FQDN where
  domainLabels (FQDN dls) = dls
  prepend d f = FQDN ⊳ (prepend d (domainLabels f))
  domainHead (FQDN dls) = domainHead dls

instance Printable FQDN where
  print (FQDN dls) = P.text $ toText dls ⊕ "."

instance Interpret FQDN where
  autoWith iopts = __parseFQDN' ⊳ autoWith iopts

parseFQDN ∷ (Printable ρ, AsFQDNError ε, MonadError ε η) ⇒ ρ → η FQDN
parseFQDN (toText → t) =
  case unsnoc t of
    Nothing →
      throwAsFQDNError DomainEmptyErr
    Just (d,'.') →
      either throwAsFQDNError (return ∘ FQDN) (parseDomainLabels' d)
    Just _ →
      throwAsFQDNError $ FQDNNotFullyQualifiedErr t

parseFQDN' ∷ MonadError FQDNError η ⇒ Text → η FQDN
parseFQDN' = parseFQDN

__parseFQDN ∷ Text → FQDN
__parseFQDN = __right ∘ parseFQDN'

__parseFQDN' ∷ Text → FQDN
__parseFQDN' = __parseFQDN

instance FromJSON FQDN where
  parseJSON (String t) = either (fail ∘ toString) return $ parseFQDN' t
  parseJSON invalid    = typeMismatch "FQDN" invalid

fqdn ∷ QuasiQuoter
fqdn = let parseExp ∷ String → ExpQ
           parseExp = appE (varE '__parseFQDN') ∘ litE ∘ stringL
        in mkQuasiQuoterExp "fqdn" parseExp

-- that's all, folks! ----------------------------------------------------------
