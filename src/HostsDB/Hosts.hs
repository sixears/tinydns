{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Hosts
  ( Hosts( Hosts ), aliases, dns_servers, domain, hosts, hostsHosts, hostIPv4
  , hostIPv4', hostIPv4s, lookupHost, mail_servers )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON )

-- base --------------------------------

import Data.Either    ( Either( Left, Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($), flip )
import Data.Functor   ( fmap )
import Data.List      ( intercalate )
import Data.Maybe     ( Maybe, maybe )
import GHC.Generics   ( Generic )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toText )

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

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- text --------------------------------

import Data.Text  ( Text )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host          ( Host, hname, ipv4 )
import HostsDB.LHostMap      ( LHostMap, lhmHosts, unLHostMap )
import HostsDB.LocalnameMap  ( LocalnameMap )

--------------------------------------------------------------------------------

data Hosts = Hosts { _domain       ∷ FQDN
                   , _hosts        ∷ LHostMap
                   , _dns_servers  ∷ [Localname]
                   , _mail_servers ∷ [Localname]
                   , _aliases      ∷ LocalnameMap
                   }
  deriving (Eq, FromJSON, Generic)

domain       ∷ Lens' Hosts FQDN
domain       = lens _domain (\ hs d → hs { _domain = d })

hosts        ∷ Lens' Hosts LHostMap
hosts        = lens _hosts (\ hs lhm → hs { _hosts = lhm })

dns_servers  ∷ Lens' Hosts [Localname]
dns_servers  = lens _dns_servers (\ hs ds → hs { _dns_servers = ds })

mail_servers ∷ Lens' Hosts [Localname]
mail_servers = lens _mail_servers (\ hs ms → hs { _mail_servers = ms })

aliases       ∷ Lens' Hosts LocalnameMap
aliases       = lens _aliases (\ hs as → hs { _aliases = as })

hostsType ∷ Type Hosts
hostsType = record $ Hosts ⊳ field "domain"       auto
                           ⊵ field "hosts"        auto
                           ⊵ field "dns_servers"  (D.list auto)
                           ⊵ field "mail_servers" (D.list auto)
                           ⊵ field "aliases"      auto

instance Interpret Hosts where
  autoWith _ = hostsType

instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:        " ⊕ show (h ⊣ hosts)
                            , "DNS_SERVERS:  " ⊕ show (h ⊣ dns_servers)
                            , "MAIL_SERVERS: " ⊕ show (h ⊣ mail_servers)
                            , "ALIASES:      " ⊕ show (h ⊣ aliases)
                            ]

lookupHost ∷ Hosts → Localname → Maybe Host
lookupHost = flip HashMap.lookup ∘ unLHostMap ∘ view hosts

hostIPv4 ∷ Hosts → Localname → Maybe IP4
hostIPv4 hs h = view ipv4 ⊳ lookupHost hs h

hostIPv4' ∷ Hosts → Localname → Either Text IP4
hostIPv4' hs h = let quote t = "'" ⊕ toText t ⊕ "'"
                     noSuchH = "hostIPv4': no such host " ⊕ quote h
                 in maybe (Left noSuchH) Right $ hostIPv4 hs h

hostsHosts ∷ Hosts → [Host]
hostsHosts = lhmHosts ∘ view hosts

hostIPv4s ∷ Hosts → [(Hostname,IP4)]
hostIPv4s = fmap ( \ h → (h ⊣ hname, h ⊣ ipv4) ) ∘ hostsHosts

-- that's all, folks! ----------------------------------------------------------
