{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module DomainNames.Error.FQDNError
  ( AsFQDNError( _FQDNError ), FQDNError( FQDNNotFullyQualifiedErr )
  , ToFQDNError( toFQDNError ), throwAsFQDNError )
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

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )

--------------------------------------------------------------------------------

data FQDNError = FQDNNotFullyQualifiedErr Text
               | DomainErrorErr DomainError
  deriving (Eq, Show)

instance Exception FQDNError

instance Printable FQDNError where
  print (FQDNNotFullyQualifiedErr t) =
    P.text $ [fmt|FQDN not fully qualified: '%t'|] t
  print (DomainErrorErr e) = print e

_FQDNNotFullyQualifiedErr ∷ Prism' FQDNError Text
_FQDNNotFullyQualifiedErr = prism' FQDNNotFullyQualifiedErr
                                   ( \ case
                                         (FQDNNotFullyQualifiedErr t) → Just t
                                         _                            → Nothing
                                   )
                    
_DomainErrorErr ∷ Prism' FQDNError DomainError
_DomainErrorErr = prism' DomainErrorErr
                         (\ case (DomainErrorErr e) → Just e; _ → Nothing)

--------------------

class AsFQDNError ε where
  _FQDNError ∷ Prism' ε FQDNError

instance AsFQDNError FQDNError where
  _FQDNError = id

instance AsDomainError FQDNError where
  _DomainError = prism' DomainErrorErr (⩼ _DomainErrorErr)

--------------------

class ToFQDNError α where
  toFQDNError ∷ α → FQDNError

instance ToFQDNError FQDNError where
  toFQDNError = id

instance ToFQDNError DomainError where
  toFQDNError = DomainErrorErr

throwAsFQDNError ∷ (ToFQDNError α, AsFQDNError ε, MonadError ε η) ⇒ α → η β
throwAsFQDNError = throwError ∘ (_FQDNError #) ∘ toFQDNError
    
-- that's all, folks! ----------------------------------------------------------
