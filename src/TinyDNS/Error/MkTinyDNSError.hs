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
import Data.Function      ( ($), (&) )
import Data.Maybe         ( Maybe( Just, Nothing ) )
import Text.Show          ( Show )

-- base-unicode-functions --------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- dhall-plus --------------------------

import DhallPlus.Error    ( AsDhallError( _DhallError )
                          , AsDhallIOError( _DhallIOError ), DhallIOError
                          , _DIEDhallError
                          )
-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )

-- fpath -------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathError )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- hostsdb -----------------------------

import HostsDB.Error.HostsError  ( AsHostsError( _HostsError ), HostsError )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣), (⊢), (⩼) )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError, _ECExecE )

-- stdmain -----------------------------

import StdMain.UsageError  ( AsUsageError( _UsageError ), UsageError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

data MkTinyDNSError = MTDDhallIOError    DhallIOError
                    | MTDExecCreateError ExecCreateError
                    | MTDHostsError      HostsError
                    | MTDDomainError     DomainError
                    | MTDFPathError      FPathError
                    | MTDUsageError      UsageError
  deriving (Eq, Show)

instance Exception MkTinyDNSError

instance Printable MkTinyDNSError where
  print (MTDDhallIOError    dioe) = print dioe
  print (MTDExecCreateError ece)  = print ece
  print (MTDHostsError      he)   = print he
  print (MTDDomainError     de)   = print de
  print (MTDFPathError      fpe)  = print fpe
  print (MTDUsageError      ue)   = print ue

instance HasCallstack MkTinyDNSError where
  callstack = lens (\ case (MTDDhallIOError    dioe) → dioe ⊣ callstack
                           (MTDExecCreateError ece)  → ece  ⊣ callstack
                           (MTDHostsError      he)   → he   ⊣ callstack
                           (MTDDomainError     de)   → de   ⊣ callstack
                           (MTDFPathError      fpe)  → fpe  ⊣ callstack
                           (MTDUsageError      ue)   → ue   ⊣ callstack
                   )
                   (\ mtde cs →
                       case mtde of
                         (MTDDhallIOError dioe) →
                           MTDDhallIOError $ dioe & callstack ⊢ cs
                         (MTDExecCreateError ece) →
                           MTDExecCreateError $ ece & callstack ⊢ cs
                         (MTDHostsError he) →
                           MTDHostsError $ he & callstack ⊢ cs
                         (MTDDomainError de) →
                           MTDDomainError $ de & callstack ⊢ cs
                         (MTDFPathError fpe) →
                           MTDFPathError $ fpe & callstack ⊢ cs
                         (MTDUsageError ue) →
                           MTDUsageError $ ue & callstack ⊢ cs
                   )

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

instance AsFPathError MkTinyDNSError where
  _FPathError = prism (_FPathError #)
                      (\ case MTDFPathError e → Right e; e → Left e)

instance AsUsageError MkTinyDNSError where
  _UsageError = prism (_UsageError #)
                      (\ case MTDUsageError e → Right e; e → Left e)

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
