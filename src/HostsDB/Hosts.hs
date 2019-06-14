{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Hosts
  ( Domains( Domains ), Hosts( Hosts )
  , aliases, aliasHosts, dnsServers, domains, hosts, hostsHosts
  , hostIPv4, hostIPv4', hostIPv4s, inAddr, lookupHost, lookupHost', mailServers
  , subDomain
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Data.List      ( intercalate )
import Data.Maybe     ( maybe )
import GHC.Generics   ( Generic )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( Interpret( autoWith ), Type, auto, field, record )

-- domainnames -------------------------

import DomainNames.FQDN      ( FQDN )
import DomainNames.Hostname  ( Hostname, Localname )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Functor      ( (⊳) )
import Fluffy.IP4          ( IP4 )
import Fluffy.MonadError   ( mapMError )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- unordered-containers ----------------

import Data.HashMap.Strict  ( HashMap, lookup, traverseWithKey )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Error.HostsError  ( AsHostsError, HostsError
                                 , aliasNotFound, localnameNotFound )
import HostsDB.Host              ( Host, hname, ipv4 )
import HostsDB.LHostMap          ( LHostMap, lhmHosts, unLHostMap )
import HostsDB.LocalnameMap      ( LocalnameMap, unLHMap )

--------------------------------------------------------------------------------

class HasSubDomain α where
  subDomain ∷ Lens' α FQDN

class HasINAddr α where
  inAddr ∷ Lens' α FQDN

data Domains = Domains { _subDomain ∷ FQDN, _inAddr ∷ FQDN }
  deriving (Eq, FromJSON, Generic, Show)

instance HasSubDomain Domains where
  subDomain = lens _subDomain (\ d s → d { _subDomain = s })

instance HasINAddr Domains where
  inAddr = lens _inAddr (\ d i → d { _inAddr = i })

instance Interpret Domains where
  autoWith _ = record $ Domains ⊳ field "sub_domain" auto ⊵ field "in_addr" auto

data Hosts = Hosts { _domains      ∷ Domains
                   , _hosts        ∷ LHostMap
                   , _dnsServers   ∷ [Localname]
                   , _mailServers  ∷ [Localname]
                   , _aliases      ∷ LocalnameMap
                   }
  deriving (Eq, FromJSON, Generic)

domains      ∷ Lens' Hosts Domains
domains      = lens _domains (\ hs d → hs { _domains = d })

hosts        ∷ Lens' Hosts LHostMap
hosts        = lens _hosts (\ hs lhm → hs { _hosts = lhm })

dnsServers  ∷ Lens' Hosts [Localname]
dnsServers  = lens _dnsServers (\ hs ds → hs { _dnsServers = ds })

mailServers ∷ Lens' Hosts [Localname]
mailServers = lens _mailServers (\ hs ms → hs { _mailServers = ms })

aliases       ∷ Lens' Hosts LocalnameMap
aliases       = lens _aliases (\ hs as → hs { _aliases = as })

hostsType ∷ Type Hosts
hostsType = record $ Hosts ⊳ field "domains"      auto
                           ⊵ field "hosts"        auto
                           ⊵ field "dns_servers"  (D.list auto)
                           ⊵ field "mail_servers" (D.list auto)
                           ⊵ field "aliases"      auto

instance HasSubDomain Hosts where
  subDomain = domains ∘ subDomain

instance HasINAddr Hosts where
  inAddr = domains ∘ inAddr

instance Interpret Hosts where
  autoWith _ = hostsType

instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:       " ⊕ show (h ⊣ hosts)
                            , "DNSSERVERS:  " ⊕ show (h ⊣ dnsServers)
                            , "MAILSERVERS: " ⊕ show (h ⊣ mailServers)
                            , "ALIASES:     " ⊕ show (h ⊣ aliases)
                            ]

lookupHost ∷ (AsHostsError ε, MonadError ε η) ⇒ Hosts → Localname → η Host
lookupHost hs l = let hs' = unLHostMap $ view hosts hs
                   in maybe (localnameNotFound l) return $ lookup l hs'

lookupHost' ∷ MonadError HostsError η ⇒ Hosts → Localname → η Host
lookupHost' = lookupHost

hostIPv4 ∷ (AsHostsError ε, MonadError ε η) ⇒ Hosts → Localname → η IP4
hostIPv4 hs h = view ipv4 ⊳ lookupHost hs h

hostIPv4' ∷ MonadError HostsError η ⇒ Hosts → Localname → η IP4
hostIPv4' = hostIPv4

hostsHosts ∷ Hosts → [Host]
hostsHosts = lhmHosts ∘ view hosts

hostIPv4s ∷ Hosts → [(Hostname,IP4)]
hostIPv4s = fmap ( \ h → (h ⊣ hname, h ⊣ ipv4) ) ∘ hostsHosts

{- | a map from local alias names to the underlying host (if any) -}
aliasHosts ∷ (AsHostsError ε, MonadError ε η) ⇒ Hosts → η (HashMap Localname Host)
aliasHosts hs =
  traverseWithKey (\ l a → mapMError (aliasNotFound l) $ lookupHost hs a)
                  (unLHMap (view aliases hs))

-- that's all, folks! ----------------------------------------------------------
