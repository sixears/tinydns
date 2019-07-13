{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.Host
  ( Host( Host ), HostComment( HostComment ), HostDesc( HostDesc )
  , comments, desc, hname, hostType, ipv4 )
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

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import Dhall  ( Interpret( autoWith ), Type, auto, field, record, strictText )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Equalish     ( Equalish( (≏) ) )
import Fluffy.Functor      ( (⊳) )
import Fluffy.IP4          ( IP4 )
import Fluffy.MACAddress   ( MACAddress )
import Fluffy.Printable    ( parenthesize )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- text --------------------------------

import qualified  Data.Text  as  Text

import Data.Text  ( Text, intercalate, pack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

--------------------------------------------------------------------------------

newtype HostDesc = HostDesc { unHostDesc ∷ Text }
  deriving (Eq, FromJSON, Generic, NFData, Show)

instance Printable HostDesc where
  print = print ∘ unHostDesc

instance IsString HostDesc where
  fromString = HostDesc ∘ pack

instance Interpret HostDesc where
  autoWith _ = HostDesc ⊳ strictText

------------------------------------------------------------

newtype HostComment = HostComment { unHostComment ∷ Text }
  deriving (Eq, FromJSON, Generic, NFData, Show)

instance Printable HostComment where
  print = print ∘ unHostComment

instance Interpret HostComment where
  autoWith _ = HostComment ⊳ strictText

instance IsString HostComment where
  fromString = HostComment ∘ pack

------------------------------------------------------------

data Host = Host { _hname    ∷ Hostname
                 , _ipv4     ∷ IP4
                 , _desc     ∷ HostDesc
                 , _comments ∷ [HostComment]
                 , _mac      ∷ Maybe MACAddress
                 }
  deriving (Eq, FromJSON, Generic, NFData, Show)

hname    ∷ Lens' Host Hostname
hname    = lens _hname (\ h n → h { _hname = n })

ipv4     ∷ Lens' Host IP4
ipv4    = lens _ipv4 (\ h i → h { _ipv4 = i })

desc     ∷ Lens' Host HostDesc
desc    = lens _desc (\ h d → h { _desc = d })

comments ∷ Lens' Host [HostComment]
comments    = lens _comments (\ h cs → h { _comments = cs })

mac      ∷ Lens' Host (Maybe MACAddress)
mac    = lens _mac (\ h m → h { _mac = m })


instance Equalish Host where
  a ≏ b = let cmp' ∷ Eq α ⇒ (α → Text) → Text → α → α → [Text]
              cmp' f t x y = if x ≡ y
                             then []
                             else [ [fmtT|%t '%t' differs from '%t'|]
                                            t (f x)             (f y) ]
              cmp ∷ (Eq α, Printable α) ⇒ Text → α → α → [Text]
              cmp = cmp' toText
           in nonEmpty $ List.concat [ cmp "Hostname"      (a ⊣ hname) (b ⊣ hname)
                                     , cmp "IPv4"          (a ⊣ ipv4) (b ⊣ ipv4)
                                     , cmp "Description"   (a ⊣ desc) (b ⊣ desc)
                                     , cmp' ([fmt|[%L]|])
                                            "Comments"
                                                   (a ⊣ comments) (b ⊣ comments)
                                     , cmp' (maybe "Nothing"
                                                      (("Just " ⊕) ∘ toText))
                                            "MACAddress"     (a ⊣ mac) (b ⊣ mac)
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
  print h = P.text $
              Text.concat [ [fmt|%T %T (%T)|] (h ⊣ hname) (h ⊣ ipv4) (h ⊣ desc)
                          , maybe "" ((" " ⊕) ∘ parenthesize ∘ toText) (h ⊣ mac)
                          , case h ⊣ comments of
                              [] → ""
                              cs → " # " ⊕ intercalate " // " (toText ⊳ cs)
                          ]

-- that's all, folks! ----------------------------------------------------------
