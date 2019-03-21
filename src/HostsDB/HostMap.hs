{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.HostMap
  ( HostMap( HostMap ), hmHosts, unHostMap )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON, Value( Object ), parseJSON, typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, mapM, return )
import Data.Either    ( Either( Left, Right ) )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Data.Monoid    ( Monoid )
import Data.Tuple     ( uncurry )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( Interpret( autoWith ), Type )

-- domainnames -------------------------

import DomainNames.Hostname  ( Localname, hostlocal, parseLocalname' )

-- fluffy ------------------------------

import Fluffy.Either     ( leftFail )
import Fluffy.Foldable2  ( HasLength( length ) )
import Fluffy.Functor2   ( (⊳) )
import Fluffy.Map        ( __fromList, fromList )
import Fluffy.Monad      ( (≫), returnPair )
import Fluffy.Text2      ( parenthesize )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex )
                             )

-- text --------------------------------

import Data.Text  ( Text, concat, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host  ( Host, hname, hostType )

--------------------------------------------------------------------------------

newtype HostMap = HostMap { unHostMap ∷ HashMap.HashMap Localname Host }
  deriving (Eq, Show)

instance HasLength HostMap where
  length = length ∘ unHostMap

data LocalHostRelation = LocalHostRelation { lname ∷ Localname, lhost ∷ Host }
  deriving Eq

instance Printable LocalHostRelation where
  print lh = P.text ∘ parenthesize $ concat [ toText $ lname lh
                                            , " → "
                                            , toText $ lhost lh
                                            ]

type instance Element HostMap = LocalHostRelation
instance MonoFoldable HostMap where
  ofoldMap ∷ Monoid ξ ⇒ (LocalHostRelation → ξ) → HostMap → ξ
  ofoldMap f hm =
    HashMap.foldlWithKey' (\ a k v → a ⊕ f (LocalHostRelation k v)) ∅ hm

  ofoldr ∷ (LocalHostRelation → α → α) → α → HostMap → α
  ofoldr f init (HostMap hm) =
    HashMap.foldrWithKey (\ k v a → f (LocalHostRelation k v) a) init hm

  ofoldl' ∷ (α → LocalHostRelation → α) → α → HostMap → α
  ofoldl' f init (HostMap hm) =
    HashMap.foldlWithKey' (\ a k v → f a (LocalHostRelation k v)) init hm

  ofoldr1Ex ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
            → HostMap → LocalHostRelation
  ofoldr1Ex f (HostMap hm) =
    ofoldr1Ex f (uncurry LocalHostRelation ⊳ HashMap.toList hm)

  ofoldl1Ex' ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
             → HostMap → LocalHostRelation
  ofoldl1Ex' f (HostMap hm) =
    ofoldl1Ex' f (uncurry LocalHostRelation ⊳ HashMap.toList hm)


instance FromJSON HostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (Localname, Host)
        go (k,v@(Object _)) = returnPair (leftFail $ parseLocalname' k, parseJSON v)
        go (k,invalid)  =
          typeMismatch (unpack $ "Host: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ (mapM go $ HashMap.toList hm) ≫ \ case
          Left  dups → fail $ toString dups
          Right hm'  → return $ HostMap hm'
  parseJSON invalid = typeMismatch "host map" invalid

hmHosts ∷ HostMap → [Host]
hmHosts (HostMap hm) = HashMap.elems hm

hostMapType ∷ Type HostMap
hostMapType = let localHNKey h = (hostlocal (hname h), h)
               in HostMap ∘ __fromList ∘ fmap localHNKey ⊳ D.list hostType

instance Interpret HostMap where
  autoWith _ = hostMapType

-- that's all, folks! ----------------------------------------------------------
