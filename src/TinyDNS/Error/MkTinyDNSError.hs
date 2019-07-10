{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module TinyDNS.Error.MkTinyDNSError
  ( MkTinyDNSError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Either        ( Either( Left, Right ) )
import Data.Eq            ( Eq )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Text.Show          ( Show )

-- base-unicode-functions --------------

import Data.Function.Unicode  ( (∘) )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )

-- fluffy ------------------------------

import Fluffy.Dhall.Error    ( AsDhallError( _DhallError )
                             , AsDhallIOError( _DhallIOError ), DhallIOError,
                             _DIEDhallError
                             )
import Fluffy.IO.Error       ( AsIOError( _IOError ), IOError )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( AsHostsError( _HostsError ), HostsError )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⩼) )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError, _ECExecE )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data MkTinyDNSError = MTDDhallIOError    DhallIOError
                    | MTDExecCreateError ExecCreateError
                    | MTDHostsError      HostsError
                    | MTDDomainError     DomainError
  deriving (Eq, Show)

instance Exception MkTinyDNSError


_MTDDhallIOError ∷ Prism' MkTinyDNSError DhallIOError
_MTDDhallIOError = prism MTDDhallIOError
                         (\ case MTDDhallIOError e → Right e; e → Left e)

_MTDExecCreateError ∷ Prism' MkTinyDNSError ExecCreateError
_MTDExecCreateError = prism MTDExecCreateError
                            (\ case MTDExecCreateError e → Right e; e → Left e)

instance AsDhallIOError MkTinyDNSError where
  _DhallIOError = _MTDDhallIOError

instance AsDhallError MkTinyDNSError where
  _DhallError = prism (_MTDDhallIOError ∘ _DIEDhallError #)
                      (\ e → case e ⩼ _MTDDhallIOError ∘ _DIEDhallError of
                               Just de → Right de
                               Nothing → Left  e
                      )


instance AsIOError MkTinyDNSError where
  _IOError = let pp ∷ Prism' MkTinyDNSError IOError
                 pp = _MTDDhallIOError ∘ _IOError
              in prism (pp#) (\ e → case e ⩼ pp of
                                      Just ioe → Right ioe
                                      Nothing  → Left  e
                             )

instance AsDomainError MkTinyDNSError where
  _DomainError = prism (_DomainError #)
                       (\ case MTDDomainError e → Right e; e → Left e)


instance AsHostsError MkTinyDNSError where
  _HostsError = prism (_HostsError #)
                      (\ case MTDHostsError e → Right e; e → Left e)

instance AsExecError MkTinyDNSError where
  _ExecError = prism (_ExecError #)
                     (\ e → case e ⩼ _MTDExecCreateError ∘ _ECExecE of
                              Just ee → Right ee
                              Nothing → Left  e
                     )


instance AsCreateProcError MkTinyDNSError where
  _CreateProcError = let pp ∷ Prism' MkTinyDNSError CreateProcError
                         pp = _MTDExecCreateError ∘ _CreateProcError
                      in prism (pp #) (\ e → case e ⩼ pp of
                                               Just cpe → Right cpe
                                               Nothing  → Left  e
                                      )

-- that's all, folks! ----------------------------------------------------------
