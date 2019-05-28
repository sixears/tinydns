{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE UnicodeSyntax #-}

module DomainNames.Error.ExecCreateDomainError
  ( ExecCreateDomainError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Text.Show          ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError( ECExecE, ECCreateE ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )
--------------------------------------------------------------------------------

data ExecCreateDomainError = ECDExecCreateE ExecCreateError
                           | ECDDomainE     DomainError
  deriving (Eq,Show)

instance Exception ExecCreateDomainError

instance Printable ExecCreateDomainError where
  print (ECDExecCreateE e) = P.string (show e)
  print (ECDDomainE e)     = print e

_ECDExecCreateE ∷ Prism' ExecCreateDomainError ExecCreateError
_ECDExecCreateE = prism' ECDExecCreateE (\ case (ECDExecCreateE e) → Just e; _ → Nothing)

instance AsExecError ExecCreateDomainError where
  _ExecError = prism' (ECDExecCreateE ∘ ECExecE)
                      (\ case (ECDExecCreateE (ECExecE e)) → Just e
                              _                              → Nothing)
instance AsCreateProcError ExecCreateDomainError where
  _CreateProcError = prism' (ECDExecCreateE ∘ ECCreateE)
                            (\ case (ECDExecCreateE (ECCreateE e)) → Just e
                                    _                                → Nothing)

instance AsDomainError ExecCreateDomainError where
  _DomainError = prism' ECDDomainE
                        (\ case (ECDDomainE e) → Just e; _ → Nothing)

-- that's all, folks! ----------------------------------------------------------
