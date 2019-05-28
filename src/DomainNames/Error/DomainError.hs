{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Error.DomainError
  ( AsDomainError( _DomainError ), DomainError( DomainEmptyErr
                                              , DomainLengthErr )
  , throwAsDomainError, toDomainError
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), const, id )
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

import DomainNames.Error.DomainLabelError  ( AsDomainLabelError( _DomainLabelError )
                                           , DomainLabelError )

--------------------------------------------------------------------------------

data DomainError = DomainEmptyErr
                 | DomainLengthErr Text
                 | DomainLabelErr DomainLabelError
  deriving (Eq, Show)

instance Exception DomainError

instance Printable DomainError where
  print DomainEmptyErr = P.text "empty domain"
  print (DomainLengthErr t) = P.text $ [fmt|domain too long: '%t'|] t
  print (DomainLabelErr e) = print e

_DomainEmptyErr ∷ Prism' DomainError ()
_DomainEmptyErr = prism' (const DomainEmptyErr)
                         ( \ case DomainEmptyErr → Just (); _ → Nothing )
                    
_DomainLabelErr ∷ Prism' DomainError DomainLabelError
_DomainLabelErr = prism' DomainLabelErr
                         (\ case (DomainLabelErr e) → Just e; _ → Nothing)

_DomainLengthErr ∷ Prism' DomainError Text
_DomainLengthErr = prism' DomainLengthErr
                          (\ case (DomainLengthErr e) → Just e; _ → Nothing)

--------------------

class AsDomainError ε where
  _DomainError ∷ Prism' ε DomainError

instance AsDomainError DomainError where
  _DomainError = id

instance AsDomainLabelError DomainError where
  _DomainLabelError = prism' DomainLabelErr (⩼ _DomainLabelErr)
    
--------------------

class ToDomainError α where
  toDomainError ∷ α → DomainError

instance ToDomainError DomainError where
  toDomainError = id

instance ToDomainError DomainLabelError where
  toDomainError = DomainLabelErr

throwAsDomainError ∷ (ToDomainError α, AsDomainError ε, MonadError ε η) ⇒ α → η β
throwAsDomainError = throwError ∘ (_DomainError #) ∘ toDomainError
    
-- that's all, folks! ----------------------------------------------------------
