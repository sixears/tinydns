{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Host
  ( Host( Host ), comments, desc, hname, hostType, ipv4, mac )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON )

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe, maybe )
import GHC.Generics   ( Generic )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ), Type, auto, field, record, strictText )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Functor2     ( (⊳) )
import Fluffy.IP4          ( IP4 )
import Fluffy.Text2        ( parenthesize )

-- text --------------------------------

import Data.Text  ( Text, concat, intercalate )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data Host = Host { hname    ∷ Hostname
                 , ipv4     ∷ IP4
                 , desc     ∷ Text
                 , comments ∷ [Text]
                 , mac      ∷ Maybe Text
                 }
  deriving (Eq, FromJSON, Generic, Show)

hostType ∷ Type Host
hostType = record $ Host ⊳ field "fqdn"     auto
                         ⊵ field "ipv4"     auto
                         ⊵ field "desc"     strictText
                         ⊵ field "comments" auto
                         ⊵ field "mac"      auto

instance Interpret Host where
  autoWith _ = hostType

instance Printable Host where
  print h = P.text $ concat [ [fmt|%T %T (%T)|] (hname h) (ipv4 h) (desc h)
                            , maybe "" ((" " ⊕) ∘ parenthesize) (mac h)
                            , case comments h of
                                [] → ""
                                cs → "# " ⊕ intercalate " // " cs
                            ]

-- that's all, folks! ----------------------------------------------------------
