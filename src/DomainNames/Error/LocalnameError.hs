{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Error.LocalnameError
  ( AsLocalnameError, LocalnameError( LocalnameDLErr )
  , toLocalnameError, throwAsLocalnameError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( id )
import Data.Maybe         ( Maybe( Just ) )
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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Error.DomainLabelError
  ( AsDomainLabelError( _DomainLabelError ), DomainLabelError )

--------------------------------------------------------------------------------

data LocalnameError = LocalnameDLErr DomainLabelError
  deriving (Eq, Show)

instance Exception LocalnameError

_LocalnameDLErr ∷ Prism' LocalnameError DomainLabelError
_LocalnameDLErr = prism' LocalnameDLErr (\ (LocalnameDLErr e) → Just e)

instance Printable LocalnameError where
  print (LocalnameDLErr e) = print e

class AsLocalnameError ε where
  _LocalnameError ∷ Prism' ε LocalnameError

--------------------

class ToLocalnameError α where
  toLocalnameError ∷ α → LocalnameError

instance ToLocalnameError LocalnameError where
  toLocalnameError = id

instance ToLocalnameError DomainLabelError where
  toLocalnameError = LocalnameDLErr

throwAsLocalnameError ∷ (ToLocalnameError α, AsLocalnameError ε, MonadError ε η) ⇒
                       α → η β
throwAsLocalnameError = throwError ∘ (_LocalnameError #) ∘ toLocalnameError
    
instance AsLocalnameError LocalnameError where
  _LocalnameError = id

instance AsDomainLabelError LocalnameError where
  _DomainLabelError = prism' LocalnameDLErr (⩼ _LocalnameDLErr)

-- that's all, folks! ----------------------------------------------------------
