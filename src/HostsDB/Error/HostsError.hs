{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Error.HostsError
  ( AsHostsError( _HostsError ), HostsError, HostsDomainError
  , HostsDomainExecCreateError
  , HostsDomainExecCreateIOError
  , HostsExecCreateError
  , HostsExecCreateIOError
  , aliasNotFound, danglingAlias, localnameNotFound
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Either        ( Either( Left, Right ) )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Maybe         ( Maybe( Just ) )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )
import DomainNames.Hostname           ( Localname )

-- fluffy ------------------------------

import Fluffy.IO.Error  ( AsIOError( _IOError ) )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⩼) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError, ExecCreateIOError
                                      , _ECCreateE, _ECExecE, _ECICreateE
                                      , _ECIExecE, _ECIIOE
                                      )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data HostsError = LocalnameNotFoundError Localname
                | DanglingAlias Localname Localname
  deriving (Eq, Show)

instance Exception HostsError

instance Printable HostsError where
  print (LocalnameNotFoundError l) = P.text $ [fmt|no such host '%T'|] l
  print (DanglingAlias a l)        =
    P.text $ [fmt|alias %T points to non-existent '%T'|] a l

class AsHostsError δ where
  _HostsError ∷ Prism' δ HostsError

localnameNotFound ∷ (AsHostsError ε, MonadError ε η) ⇒ Localname → η α
localnameNotFound l = throwError $ _HostsError # LocalnameNotFoundError l

danglingAlias ∷ (AsHostsError ε, MonadError ε η) ⇒ Localname → Localname → η α
danglingAlias a l = throwError $ _HostsError # DanglingAlias a l

{- | convert LocalnameNotFoundErrors to DanglingAlias (from `a`); leave other
     HostsErrors alone
 -}
aliasNotFound ∷ (AsHostsError ε) ⇒ Localname → ε → ε
aliasNotFound a e =
  case e ⩼ _HostsError  of
    Just (LocalnameNotFoundError l) → _HostsError # DanglingAlias a l
    _                               → e
                      

instance AsHostsError HostsError where
  _HostsError = id

------------------------------------------------------------

data HostsDomainError = HDHostsError  HostsError
                      | HDDomainError DomainError
  deriving (Eq, Show)

instance Exception HostsDomainError

_HDHostsError ∷ Prism' HostsDomainError HostsError
_HDHostsError = prism HDHostsError
                      (\ e → case e of HDHostsError e' → Right e'; _ → Left e)

_HDDomainError ∷ Prism' HostsDomainError DomainError
_HDDomainError = prism HDDomainError
                       (\ e → case e of HDDomainError e' → Right e'; _ → Left e)

instance AsHostsError HostsDomainError where
  _HostsError = _HDHostsError

instance AsDomainError HostsDomainError where
  _DomainError = _HDDomainError

------------------------------------------------------------

data HostsExecCreateError = HECExecCreateError ExecCreateError
                          | HECHostsError      HostsError
  deriving (Eq, Show)

instance Exception HostsExecCreateError

_HECExecCreateError ∷ Prism' HostsExecCreateError ExecCreateError
_HECExecCreateError =
  prism (\ e → HECExecCreateError e)
        (\ e → case e of HECExecCreateError e' → Right e'; _ → Left e)

_HECHostsError ∷ Prism' HostsExecCreateError HostsError
_HECHostsError = prism (\ e → HECHostsError e)
                       (\ e → case e of HECHostsError e' → Right e'; _ → Left e)

instance AsHostsError HostsExecCreateError where
  _HostsError = _HECHostsError

instance AsExecError HostsExecCreateError where
  _ExecError = _HECExecCreateError ∘ _ECExecE

instance AsCreateProcError HostsExecCreateError where
  _CreateProcError = _HECExecCreateError ∘ _ECCreateE

------------------------------------------------------------

data HostsDomainExecCreateError = HDECExecCreateError  ExecCreateError
                                | HDECHostsDomainError HostsDomainError
  deriving (Eq, Show)

instance Exception HostsDomainExecCreateError

_HDECExecCreateError ∷ Prism' HostsDomainExecCreateError ExecCreateError
_HDECExecCreateError =
  prism (\ e → HDECExecCreateError e)
        (\ e → case e of HDECExecCreateError e' → Right e'; _ → Left e)

_HDECHostsDomainError ∷ Prism' HostsDomainExecCreateError HostsDomainError
_HDECHostsDomainError =
  prism (\ e → HDECHostsDomainError e)
        (\ e → case e of HDECHostsDomainError e' → Right e'; _ → Left e)

instance AsHostsError HostsDomainExecCreateError where
  _HostsError = _HDECHostsDomainError ∘ _HDHostsError

instance AsDomainError HostsDomainExecCreateError where
  _DomainError = _HDECHostsDomainError ∘ _HDDomainError

instance AsExecError HostsDomainExecCreateError where
  _ExecError = _HDECExecCreateError ∘ _ECExecE

instance AsCreateProcError HostsDomainExecCreateError where
  _CreateProcError = _HDECExecCreateError ∘ _ECCreateE

------------------------------------------------------------

data HostsExecCreateIOError = HECIExecCreateIOError ExecCreateIOError
                            | HECIHostsError        HostsError
  deriving (Eq, Show)

_HECIExecCreateIOError ∷ Prism' HostsExecCreateIOError ExecCreateIOError
_HECIExecCreateIOError =
  prism (\ e → HECIExecCreateIOError e)
        (\ e → case e of HECIExecCreateIOError e' → Right e'; _ → Left e)

_HECIHostsError ∷ Prism' HostsExecCreateIOError HostsError
_HECIHostsError =
  prism (\ e → HECIHostsError e)
        (\ e → case e of HECIHostsError e' → Right e'; _ → Left e)

instance AsHostsError HostsExecCreateIOError where
  _HostsError = _HECIHostsError

instance Exception HostsExecCreateIOError

instance AsExecError HostsExecCreateIOError where
  _ExecError = _HECIExecCreateIOError ∘ _ECIExecE

instance AsCreateProcError HostsExecCreateIOError where
  _CreateProcError = _HECIExecCreateIOError ∘ _ECICreateE

instance AsIOError HostsExecCreateIOError where
  _IOError = _HECIExecCreateIOError ∘ _ECIIOE

------------------------------------------------------------

data HostsDomainExecCreateIOError = HDECIExecCreateIOError ExecCreateIOError
                                  | HDECIHostsDomainError  HostsDomainError
  deriving (Eq, Show)

_HDECIExecCreateIOError ∷ Prism' HostsDomainExecCreateIOError ExecCreateIOError
_HDECIExecCreateIOError =
  prism (\ e → HDECIExecCreateIOError e)
        (\ e → case e of HDECIExecCreateIOError e' → Right e'; _ → Left e)

_HDECIHostsDomainError ∷ Prism' HostsDomainExecCreateIOError HostsDomainError
_HDECIHostsDomainError =
  prism (\ e → HDECIHostsDomainError e)
        (\ e → case e of HDECIHostsDomainError e' → Right e'; _ → Left e)

instance AsHostsError HostsDomainExecCreateIOError where
  _HostsError = _HDECIHostsDomainError ∘ _HDHostsError

instance AsDomainError HostsDomainExecCreateIOError where
  _DomainError = _HDECIHostsDomainError ∘ _HDDomainError

instance Exception HostsDomainExecCreateIOError

instance AsExecError HostsDomainExecCreateIOError where
  _ExecError = _HDECIExecCreateIOError ∘ _ECIExecE

instance AsCreateProcError HostsDomainExecCreateIOError where
  _CreateProcError = _HDECIExecCreateIOError ∘ _ECICreateE

instance AsIOError HostsDomainExecCreateIOError where
  _IOError = _HDECIExecCreateIOError ∘ _ECIIOE

-- that's all, folks! ----------------------------------------------------------
