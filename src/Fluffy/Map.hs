{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Map
  ( fromList, __fromList
  )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Either    ( either )
import Data.Eq        ( Eq )
import Data.Function  ( ($), id )
import Data.Tuple     ( fst )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- mono-traversable --------------------

import Data.Containers  ( ContainerKey, IsMap, MapValue, mapFromList )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- Unique ------------------------------

import Data.List.UniqueUnsorted  ( repeated )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Functor2   ( (⊳) )
import Fluffy.Printable  ( __ERR, q )

--------------------------------------------------------------------------------

newtype RepeatedKeyError α = RepeatedKeyError [α]

instance Printable α ⇒ Printable (RepeatedKeyError α) where
  print (RepeatedKeyError ks) = P.text $ [fmt|repeated keys [%L]|] (q ⊳ ks)

------------------------------------------------------------

{-| Like `HashMap.fromList`, but throws a RepeatedKeyError if any key is
     duplicated in the incoming list
 -}
fromList ∷ (Hashable κ, Eq κ, MonadError (RepeatedKeyError κ) η,
            ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒
           [(κ,υ)] → η σ
fromList kvs = case repeated $ fst ⊳ kvs of
                  []   → return $ mapFromList kvs
                  dups → throwError $ RepeatedKeyError dups

{-| `fromList`, but uses `error` in case of duplicate keys -}
__fromList ∷ (Hashable κ, Printable κ, ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒ [(κ,υ)] → σ
__fromList = either __ERR id ∘ fromList

-- that's all, folks! ----------------------------------------------------------
