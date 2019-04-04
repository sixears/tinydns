{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Hosts
  ( Hosts( Hosts ), aliases, dns_servers, hosts, hostsHosts, hostIPv4
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
import Fluffy.Functor2     ( (⊳) )
import Fluffy.IP4          ( IP4 )

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

data Hosts = Hosts { domain       ∷ FQDN
                   , hosts        ∷ LHostMap
                   , dns_servers  ∷ [Localname]
                   , mail_servers ∷ [Localname]
                   , aliases      ∷ LocalnameMap
                   }
  deriving (Eq, FromJSON, Generic)

hostsType ∷ Type Hosts
hostsType = record $ Hosts ⊳ field "domain"       auto
                           ⊵ field "hosts"        auto
                           ⊵ field "dns_servers"  (D.list auto)
                           ⊵ field "mail_servers" (D.list auto)
                           ⊵ field "aliases"      auto

instance Interpret Hosts where
  autoWith _ = hostsType

instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:        " ⊕ show (hosts h)
                            , "DNS_SERVERS:  " ⊕ show (dns_servers h)
                            , "MAIL_SERVERS: " ⊕ show (mail_servers h)
                            , "ALIASES:      " ⊕ show (aliases h)
                            ]

lookupHost ∷ Hosts → Localname → Maybe Host
lookupHost = flip HashMap.lookup ∘ unLHostMap ∘ hosts

hostIPv4 ∷ Hosts → Localname → Maybe IP4
hostIPv4 hs h = ipv4 ⊳ lookupHost hs h

hostIPv4' ∷ Hosts → Localname → Either Text IP4
hostIPv4' hs h = let quote t = "'" ⊕ toText t ⊕ "'"
                     noSuchH = "hostIPv4': no such host " ⊕ quote h
                 in maybe (Left noSuchH) Right $ hostIPv4 hs h

hostsHosts ∷ Hosts → [Host]
hostsHosts = lhmHosts ∘ hosts

hostIPv4s ∷ Hosts → [(Hostname,IP4)]
hostIPv4s = fmap ( \ h → (hname h, ipv4 h) ) ∘ hostsHosts

-- that's all, folks! ----------------------------------------------------------
