{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Textual
  ( parseJSONString, parseTextM )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( Value( String ), Parser, typeMismatch )

-- base --------------------------------

import Control.Monad  ( Monad, fail, return )
import Data.Function  ( ($) )

-- data-textual ------------------------

import Data.Textual           ( Parsed( Parsed, Malformed )
                              , Textual, parseText, toString )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

{- | `parseText`, but in a Monadic context, so that a parse failure becomes a
     `fail`
 -}
parseTextM ∷ (Textual α, Monad η) ⇒ Text → Text → η α
parseTextM s t =
  case parseText t of
    Parsed    mac → return mac
    Malformed _ e → fail $ [fmt|failed to parse %t: %T (%t)|] s e t


parseJSONString ∷ Textual α ⇒ Text → Value → Parser α
parseJSONString s (String t) = parseTextM s t
parseJSONString s invalid    = typeMismatch (toString s) invalid



-- that's all, folks! ----------------------------------------------------------
