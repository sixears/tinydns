{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module HostsDB.LHostMap
  ( LHostMap( LHostMap ), LocalHostRelation( LocalHostRelation, lname, lhost )
  , lhmHosts, unLHostMap )
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

import Data.Textual  ( Printable( print ), toString )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( Interpret( autoWith ), Type )

-- domainnames -------------------------

import DomainNames.Hostname  ( Localname, hostlocal, parseLocalname' )

-- fluffy ------------------------------

import Fluffy.Either    ( leftFail )
import Fluffy.Foldable  ( HasLength( length ) )
import Fluffy.Functor   ( (⊳) )
import Fluffy.Map       ( __fromList, fromList )
import Fluffy.Monad     ( (≫), returnPair )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host  ( Host, hname, hostType )

--------------------------------------------------------------------------------

newtype LHostMap = LHostMap { unLHostMap ∷ HashMap.HashMap Localname Host }
  deriving (Eq, NFData, Show)

instance HasLength LHostMap where
  length = length ∘ unLHostMap

data LocalHostRelation = LocalHostRelation { lname ∷ Localname, lhost ∷ Host }
  deriving Eq

instance Printable LocalHostRelation where
  print (LocalHostRelation nm hst) = P.text $ [fmt|(%T → %T)|] nm hst

type instance Element LHostMap = LocalHostRelation
instance MonoFoldable LHostMap where
  ofoldMap ∷ Monoid ξ ⇒ (LocalHostRelation → ξ) → LHostMap → ξ
  ofoldMap f hm =
    HashMap.foldlWithKey' (\ a k v → a ⊕ f (LocalHostRelation k v)) ∅ hm

  ofoldr ∷ (LocalHostRelation → α → α) → α → LHostMap → α
  ofoldr f init (LHostMap hm) =
    HashMap.foldrWithKey (\ k v a → f (LocalHostRelation k v) a) init hm

  ofoldl' ∷ (α → LocalHostRelation → α) → α → LHostMap → α
  ofoldl' f init (LHostMap hm) =
    HashMap.foldlWithKey' (\ a k v → f a (LocalHostRelation k v)) init hm

  ofoldr1Ex ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
            → LHostMap → LocalHostRelation
  ofoldr1Ex f (LHostMap hm) =
    ofoldr1Ex f (uncurry LocalHostRelation ⊳ HashMap.toList hm)

  ofoldl1Ex' ∷ (LocalHostRelation → LocalHostRelation → LocalHostRelation)
             → LHostMap → LocalHostRelation
  ofoldl1Ex' f (LHostMap hm) =
    ofoldl1Ex' f (uncurry LocalHostRelation ⊳ HashMap.toList hm)


instance FromJSON LHostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (Localname, Host)
        go (k,v@(Object _)) =
          returnPair (leftFail $ parseLocalname' k, parseJSON v)
        go (k,invalid)      =
          typeMismatch (unpack $ "Host: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ mapM go (HashMap.toList hm) ≫ \ case
          Left  dups → fail $ toString dups
          Right hm'  → return $ LHostMap hm'
  parseJSON invalid = typeMismatch "host map" invalid

lhmHosts ∷ LHostMap → [Host]
lhmHosts (LHostMap hm) = HashMap.elems hm

hostMapType ∷ Type LHostMap
hostMapType = let localHNKey h = (hostlocal (h ⊣ hname), h)
               in LHostMap ∘ __fromList ∘ fmap localHNKey ⊳ D.list hostType

instance Interpret LHostMap where
  autoWith _ = hostMapType

-- that's all, folks! ----------------------------------------------------------
