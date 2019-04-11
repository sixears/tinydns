{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Host
  ( Host( Host ), HostComment( HostComment ), HostDesc( HostDesc )
  , comments, desc, hname, hostType, ipv4, mac )
where

-- aeson -------------------------------

import Data.Aeson  ( FromJSON )

-- base --------------------------------

import qualified  Data.List  as  List

import Data.Eq             ( Eq )
import Data.Function       ( ($) )
import Data.List.NonEmpty  ( nonEmpty )
import Data.Maybe          ( Maybe, maybe )
import Data.String         ( IsString( fromString ) )
import GHC.Generics        ( Generic )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ), Type, auto, field, record, strictText )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Equalish     ( Equalish( (≏) ) )
import Fluffy.Functor      ( (⊳) )
import Fluffy.IP4          ( IP4 )
import Fluffy.MACAddress   ( MACAddress )
import Fluffy.Printable    ( parenthesize )

-- text --------------------------------

import qualified  Data.Text  as  Text

import Data.Text  ( Text, intercalate, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

--------------------------------------------------------------------------------

newtype HostDesc = HostDesc { unHostDesc ∷ Text }
  deriving (Eq, FromJSON, Generic, Show)

instance Printable HostDesc where
  print = print ∘ unHostDesc

instance IsString HostDesc where
  fromString = HostDesc ∘ pack

instance Interpret HostDesc where
  autoWith _ = HostDesc ⊳ strictText

------------------------------------------------------------

newtype HostComment = HostComment { unHostComment ∷ Text }
  deriving (Eq, FromJSON, Generic, Show)

instance Printable HostComment where
  print = print ∘ unHostComment

instance Interpret HostComment where
  autoWith _ = HostComment ⊳ strictText

instance IsString HostComment where
  fromString = HostComment ∘ pack

------------------------------------------------------------

data Host = Host { hname    ∷ Hostname
                 , ipv4     ∷ IP4
                 , desc     ∷ HostDesc
                 , comments ∷ [HostComment]
                 , mac      ∷ Maybe MACAddress
                 }
  deriving (Eq, FromJSON, Generic, Show)

instance Equalish Host where
  a ≏ b = let cmp' ∷ Eq α ⇒ (α → Text) → Text → α → α → [Text]
              cmp' f t x y = if x ≡ y
                             then []
                             else [ [fmtT|%t '%t' differs from '%t'|]
                                            t (f x)             (f y) ]
              cmp ∷ (Eq α, Printable α) ⇒ Text → α → α → [Text]
              cmp = cmp' toText
           in nonEmpty $ List.concat [ cmp "Hostname"        (hname a) (hname b)
                                     , cmp "IPv4"            (ipv4  a) (ipv4  b)
                                     , cmp "Description"     (desc  a) (desc  b)
                                     , cmp' -- (\ xs → [fmt|[%L]|] (unHostComments xs))
                                            ([fmt|[%L]|])
                                            "Comments"
                                                     (comments  a) (comments  b)
                                     , cmp' (maybe "Nothing"
                                                      (("Just " ⊕) ∘ toText))
                                            "MACAddress"     (mac   a) (mac   b)
                                     ]

hostType ∷ Type Host
hostType = record $ Host ⊳ field "fqdn"     auto
                         ⊵ field "ipv4"     auto
                         ⊵ field "desc"     auto
                         ⊵ field "comments" auto
                         ⊵ field "mac"      auto

instance Interpret Host where
  autoWith _ = hostType

instance Printable Host where
  print h = P.text $ Text.concat [ [fmt|%T %T (%T)|] (hname h) (ipv4 h) (desc h)
                                 , maybe "" ((" " ⊕) ∘ parenthesize ∘ toText)
                                            (mac h)
                                 , case comments h of
                                     [] → ""
                                     cs → " # " ⊕ intercalate " // " (toText ⊳ cs)
                                 ]

-- that's all, folks! ----------------------------------------------------------
