{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Error.HostnameError
  ( AsHostnameError, HostnameError( HostnameNotFullyQualifiedE )
  , throwAsHostnameError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fluffy ------------------------------

import Fluffy.Lens  ( (⩼) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Error.DomainError  ( DomainError )
import DomainNames.Error.FQDNError    ( AsFQDNError( _FQDNError ), FQDNError
                                      , toFQDNError )

--------------------------------------------------------------------------------

data HostnameError = HostnameNotFullyQualifiedE Text
                   | HostnameFQDNE FQDNError
  deriving (Eq, Show)

instance Exception HostnameError

_HostnameNotFullyQualifiedE ∷ Prism' HostnameError Text
_HostnameNotFullyQualifiedE =
  prism' HostnameNotFullyQualifiedE
         ( \ case (HostnameNotFullyQualifiedE h) → Just h; _ → Nothing )
                    
_HostnameFQDNE ∷ Prism' HostnameError FQDNError
_HostnameFQDNE = prism' HostnameFQDNE
                          (\ case (HostnameFQDNE e) → Just e; _ → Nothing)

instance Printable HostnameError where
  print (HostnameNotFullyQualifiedE h) =
    P.text $ [fmt|hostname is not fully qualified: '%t'|] h
  print (HostnameFQDNE e) = print e

class AsHostnameError ε where
  _HostnameError ∷ Prism' ε HostnameError

--------------------

class ToHostnameError α where
  toHostnameError ∷ α → HostnameError

instance ToHostnameError HostnameError where
  toHostnameError = id

instance ToHostnameError FQDNError where
  toHostnameError = HostnameFQDNE

instance ToHostnameError DomainError where
  toHostnameError = HostnameFQDNE ∘ toFQDNError

throwAsHostnameError ∷ (ToHostnameError α, AsHostnameError ε, MonadError ε η) ⇒
                       α → η β
throwAsHostnameError = throwError ∘ (_HostnameError #) ∘ toHostnameError
    
instance AsHostnameError HostnameError where
  _HostnameError = id

instance AsFQDNError HostnameError where
  _FQDNError = prism' HostnameFQDNE (⩼ _HostnameFQDNE)

-- that's all, folks! ----------------------------------------------------------
