{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module DomainNames.Error.UQDNError
  ( AsUQDNError, UQDNError( UQDNFullyQualifiedErr ), throwAsUQDNError )
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

data UQDNError = UQDNFullyQualifiedErr Text
               | DomainErrorErr DomainError
  deriving (Eq, Show)

instance Exception UQDNError

instance Printable UQDNError where
  print (UQDNFullyQualifiedErr t) =
    P.text $ [fmt|UQDN fully qualified: '%t'|] t
  print (DomainErrorErr e) = print e

_UQDNNotFullyQualifiedErr ∷ Prism' UQDNError Text
_UQDNNotFullyQualifiedErr = prism' UQDNFullyQualifiedErr
                                   ( \ case
                                         (UQDNFullyQualifiedErr t) → Just t
                                         _                         → Nothing
                                   )
                    
_DomainErrorErr ∷ Prism' UQDNError DomainError
_DomainErrorErr = prism' DomainErrorErr
                         (\ case (DomainErrorErr e) → Just e; _ → Nothing)

--------------------

class AsUQDNError ε where
  _UQDNError ∷ Prism' ε UQDNError

instance AsUQDNError UQDNError where
  _UQDNError = id

instance AsDomainError UQDNError where
  _DomainError = prism' DomainErrorErr (⩼ _DomainErrorErr)

--------------------

class ToUQDNError α where
  toUQDNError ∷ α → UQDNError

instance ToUQDNError UQDNError where
  toUQDNError = id

instance ToUQDNError DomainError where
  toUQDNError = DomainErrorErr

throwAsUQDNError ∷ (ToUQDNError α, AsUQDNError ε, MonadError ε η) ⇒ α → η β
throwAsUQDNError = throwError ∘ (_UQDNError #) ∘ toUQDNError
    
-- that's all, folks! ----------------------------------------------------------
