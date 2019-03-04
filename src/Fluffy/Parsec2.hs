{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Fluffy.Parsec2
  ( AsParseError(..), IOParseError, Parsecable(..), digits, parens, __parsecN )
where

-- base --------------------------------

import Control.Applicative    ( pure )
import Control.Monad          ( return )
import Data.Bifunctor         ( first )
import Data.Char              ( Char )
import Data.Either            ( Either( Left, Right ) )
import Data.Function          ( ($) )
import Data.Functor.Identity  ( Identity )
import Data.Monoid            ( mappend )
import Data.String            ( String )
import Data.Word              ( Word8 )
import Text.Read              ( read )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, oneOf, string )
import Text.Parsec.Combinator  ( between, count, eof, many1 )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, parse, try )

-- path --------------------------------

import Path  ( File, Path, toFilePath )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Applicative    ( (⊵), (∤), (⋪) )
import Fluffy.Either         ( __right )
import Fluffy.Functor2       ( (⊳) )
import Fluffy.IO.Error       ( AsIOError )
import Fluffy.Lens2          ( (⋕) )
import Fluffy.Monad          ( (≫) )
import Fluffy.MonadIO        ( MonadIO, readFile )
import Fluffy.Parsec.Error2  ( AsParseError( _ParseError ), IOParseError
                             , ParseError( ParseError ) )

--------------------------------------------------------------------------------

class Parsecable χ where
  parser ∷ Stream s Identity Char ⇒ Parsec s u χ

  parsec ∷ (AsParseError ε, MonadError ε μ,
             Stream s Identity Char, Printable σ) ⇒
            σ → s → μ χ
  parsec sourceName t = case parse parser (toString sourceName) t of
                           Left  e → throwError (_ParseError ⋕ ParseError e)
                           Right s → return s

  __parsec ∷ (Printable σ, Stream s Identity Char) ⇒ σ → s → χ
  __parsec sourceName = __right ∘ parsec' sourceName

  parsec' ∷ (MonadError ParseError μ, Stream s Identity Char, Printable σ) ⇒
             σ → s → μ χ
  parsec' = parsec

  parsecFile ∷ (MonadIO μ, AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
                Path β File → μ χ
  parsecFile fn = readFile fn ≫ parsec (toFilePath fn)

  parsecFile' ∷ (MonadIO μ, MonadError IOParseError μ) ⇒ Path β File → μ χ
  parsecFile' = parsecFile

------------------------------------------------------------

__parsecN ∷ (Stream s Identity Char, Parsecable χ) ⇒ s → χ
__parsecN t = __right ∘ first ParseError $ parse (parser ⋪ eof) "" t

----------------------------------------

parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = between (char '(') (char ')')

----------------------------------------

digits ∷ Stream s m Char ⇒ ParsecT s u m String
digits = many1 digit

------------------------------------------------------------

instance Parsecable Word8 where
  {-| parse a word8 value in denary; that is, 0-255 -}
  parser = read ⊳ go
           where go = try (mappend ⊳ string "25" ⊵ (pure ⊳ oneOf "012345"))
                    ∤ try ((:) ⊳ char '2' ⊵ ((:) ⊳ oneOf "01234" ⊵ count 1 digit))
                    ∤ try ((:) ⊳ oneOf "01" ⊵ count 2 digit)
                    ∤ try (count 2 digit)
                    ∤ count 1 digit


-- that's all, folks! ----------------------------------------------------------
