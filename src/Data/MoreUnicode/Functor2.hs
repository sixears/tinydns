{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.MoreUnicode.Functor2
  ( (⩺) )
where

-- base --------------------------------

import Data.Functor  ( Functor, fmap )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

{- | Functor combinator to lift f and fmap it across the result of g.
     This may be particularly useful for lifting into a monad, with
     mono-unsaturated functions (i.e., for point-free style; e.g.,
     
       \ fn -> length <$> readFile fn
  
    may be re-written as 
  
       length <$$> readFile
 -}

infixl 4 <$$>
(<$$>) ∷ Functor φ ⇒ (β → γ) → (α → φ β) → α → φ γ
f <$$> g = fmap f ∘ g

infixl 4 ⩺
(⩺) ∷ Functor φ ⇒ (β → γ) → (α → φ β) → α → φ γ
(⩺) = (<$$>)

-- that's all, folks! ----------------------------------------------------------
