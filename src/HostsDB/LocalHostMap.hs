{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.LocalHostMap
  ( LocalHostMap( LocalHostMap ), unLHMap )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON, Value( Object, String ), typeMismatch )

-- base --------------------------------

import Control.Monad  ( fail, mapM, return )
import Data.Either    ( Either( Left, Right ), either )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( Interpret, Type, auto, autoWith, field, record )

-- domainnames -------------------------

import DomainNames.Error.LocalnameError  ( LocalnameError )
import DomainNames.Hostname              ( Localname, parseLocalname' )

-- fluffy ------------------------------

import Fluffy.Applicative  ( (⊵) )
import Fluffy.Functor2     ( (⊳) )
import Fluffy.Map          ( __fromList, fromList )
import Fluffy.Monad        ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml

--------------------------------------------------------------------------------

newtype LocalHostMap =
    LocalHostMap { unLHMap ∷ HashMap.HashMap Localname Localname }
  deriving (Eq, Show)

data LocalAlias = LocalAlias Localname Localname

localAliasType ∷ Type LocalAlias
localAliasType = record $ LocalAlias ⊳ field "from" auto
                                     ⊵ field "to"   auto

instance Interpret LocalAlias where
  autoWith _ = localAliasType

localAliasPair ∷ LocalAlias → (Localname,Localname)
localAliasPair (LocalAlias aliasFrom aliasTo) = (aliasFrom,aliasTo)

localHostMapType ∷ Type LocalHostMap
localHostMapType =
  LocalHostMap ⊳ __fromList ∘ fmap localAliasPair ⊳ D.list localAliasType

instance Interpret LocalHostMap where
  autoWith _ = localHostMapType

instance FromJSON LocalHostMap where
  parseJSON (Object hm) =
    let go' ∷ MonadError LocalnameError η ⇒
              (Text,Text) → η (Localname,Localname)
        go' (k, v) = do k' ← parseLocalname' k
                        v' ← parseLocalname' v
                        return (k',v')
        go ∷ (Text,Value) → Yaml.Parser (Localname, Localname)
        go (k,String v) = either (fail ∘ toString) return $ go' (k,v)
        go (k,invalid)  =
          typeMismatch (unpack $ "local host name: '" ⊕ k ⊕ "'") invalid
     in fromList ⊳ (mapM go $ HashMap.toList hm) ≫ \ case
          Left dups → fail $ toString dups
          Right hm' → return $ LocalHostMap hm'
  parseJSON invalid     = typeMismatch "local host map" invalid

-- that's all, folks! ----------------------------------------------------------
