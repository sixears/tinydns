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
import Data.Ord       ( Ord )
import Data.String    ( String )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ) )

-- fluffy ------------------------------

import Fluffy.Either   ( __right )
import Fluffy.Functor  ( (⊳) )
import Fluffy.Quasi    ( mkQuasiQuoterExp )

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

import DomainNames.Domain             ( Domain( domainHead, prepend )
                                      , IsDomainLabels( domainLabels )
                                      , DomainLabels, parseDomainLabels'
                                      )
import DomainNames.Error.DomainError  ( DomainError( DomainEmptyErr ) )
import DomainNames.Error.FQDNError    ( AsFQDNError
                                      , FQDNError( FQDNNotFullyQualifiedErr )
                                      , throwAsFQDNError
                                      )

--------------------------------------------------------------------------------

newtype FQDN = FQDN { unFQDN ∷ DomainLabels }
  deriving (Eq, Hashable, NFData, Ord, Show)

instance IsDomainLabels FQDN where
  domainLabels = iso unFQDN FQDN

instance Domain FQDN where
  prepend d f = FQDN ⊳ (prepend d (f ⊣ domainLabels))
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
