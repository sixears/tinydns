{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module DomainNames.Hostname
  ( Hostname, Localname, (<.>), (<..>)
  , host, hostlocal, hostname, localname, parseHostname, parseHostname'
  , __parseHostname, __parseHostname', parseLocalname, parseLocalname'
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( typeMismatch )

-- base --------------------------------

import Control.Monad   ( fail, return )
import Data.Either     ( either )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import GHC.Generics    ( Generic )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ) )

-- fluffy ------------------------------

import Fluffy.Either   ( __right )
import Fluffy.Functor  ( (⊳) )
import Fluffy.Quasi    ( mkQuasiQuoterExp )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( from, iso )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

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

import DomainNames.Domain               ( DomainLabel
                                        , IsDomainLabels( domainLabels )
                                        , domainHead, prepend, parseDomainLabel'
                                        )
import DomainNames.Error.DomainError    ( AsDomainError
                                        , DomainError( DomainEmptyErr ) )
import DomainNames.Error.LocalnameError ( AsLocalnameError, LocalnameError
                                        , throwAsLocalnameError )
import DomainNames.FQDN                 ( FQDN, parseFQDN' )

import DomainNames.Error.HostnameError
  ( AsHostnameError, HostnameError( HostnameNotFullyQualifiedE )
  , throwAsHostnameError )

--------------------------------------------------------------------------------

newtype Localname = Localname DomainLabel
  deriving (Eq,Generic,Hashable,Show)

instance Interpret Localname where
  autoWith iopts = __parseLocalname' ⊳ autoWith iopts

instance Printable Localname where
  print (Localname d) = print d

instance FromJSON Localname where
  parseJSON (String t) = either (fail ∘ toString) return $ parseLocalname' t
  parseJSON invalid    = typeMismatch "localname" invalid

parseLocalname ∷ (Printable ρ, AsLocalnameError ε, MonadError ε η) ⇒
                 ρ → η Localname
parseLocalname (toText → t) =
  either throwAsLocalnameError (return ∘ Localname) $ parseDomainLabel' t

parseLocalname' ∷ (Printable ρ, MonadError LocalnameError η) ⇒ ρ → η Localname
parseLocalname' = parseLocalname

__parseLocalname ∷ Printable ρ ⇒ ρ → Localname
__parseLocalname = __right ∘ parseLocalname'

__parseLocalname' ∷ Text → Localname
__parseLocalname' = __parseLocalname

localname ∷ QuasiQuoter
localname = let parseExp ∷ String → ExpQ
                parseExp = appE (varE '__parseLocalname') ∘ litE ∘ stringL
             in mkQuasiQuoterExp "local" parseExp

------------------------------------------------------------

newtype Hostname = Hostname { unHostname ∷ FQDN }
  deriving (Eq, Generic, Hashable, Show)

instance Printable Hostname where
  print (Hostname fq) = print fq

instance IsDomainLabels Hostname where
  domainLabels = iso (view domainLabels ∘ unHostname) (Hostname ∘ view (from domainLabels))

----------------------------------------

hostlocal ∷ Hostname → Localname
hostlocal (Hostname h) = Localname (domainHead h)

----------------------------------------

instance Interpret Hostname where
  autoWith iopts = __parseHostname' ⊳ autoWith iopts

instance FromJSON Hostname where
  parseJSON (String t) = either (fail ∘ toString) return $ parseHostname' t
  parseJSON invalid    = typeMismatch "hostname" invalid

parseHostname ∷ (Printable ρ, AsHostnameError ε, MonadError ε η) ⇒
                ρ → η Hostname
parseHostname (toText → t) =
  case unsnoc t of
    Nothing →
      throwAsHostnameError DomainEmptyErr
    Just (_,'.') →
      either throwAsHostnameError (return ∘ Hostname) $ parseFQDN' t
    Just (_,_) →
      throwAsHostnameError $ HostnameNotFullyQualifiedE t

parseHostname' ∷ (Printable ρ, MonadError HostnameError η) ⇒ ρ → η Hostname
parseHostname' = parseHostname

__parseHostname ∷ Printable ρ ⇒ ρ → Hostname
__parseHostname = __right ∘ parseHostname'

__parseHostname' ∷ Text → Hostname
__parseHostname' = __parseHostname

hostname ∷ QuasiQuoter
hostname = let parseExp ∷ String → ExpQ
               parseExp = appE (varE '__parseHostname') ∘ litE ∘ stringL
            in mkQuasiQuoterExp "hostname" parseExp
                

host ∷ QuasiQuoter
host = hostname
-- h ∷ QuasiQuoter
-- h = hostname

(<.>) ∷ MonadError DomainError η ⇒ Localname → FQDN → η Hostname
(<.>) = (<..>)

(<..>) ∷ (AsDomainError ε, MonadError ε η) ⇒ Localname → FQDN → η Hostname
(Localname d) <..> f = Hostname ⊳ (prepend d f)

-- that's all, folks! ----------------------------------------------------------
