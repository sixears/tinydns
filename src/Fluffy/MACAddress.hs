{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module Fluffy.MACAddress
  ( MACAddress( MACAddress ), mac, macAddress )
where

import Prelude  ( Double, (+), (*), fromInteger )

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON( parseJSON ), Parser, ToJSON( toJSON )
                         , Value( Array, Number, String ), typeMismatch )

-- base --------------------------------

import Control.Applicative  ( empty, pure )
import Control.Monad        ( fail, return )
import Data.Bool            ( otherwise )
import Data.Char            ( Char, isHexDigit )
import Data.Either          ( Either( Left, Right ) )
import Data.Eq              ( Eq )
import Data.Foldable        ( toList )
import Data.Function        ( ($) )
import Data.List            ( all, head )
import Data.Ord             ( Ord, (<), (>) )
import Data.Tuple           ( fst )
import Data.Word            ( Word8 )
import Numeric              ( readHex )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import qualified  Data.Textual.Integral  as  TI

import Data.Textual           ( Printable( print ), Textual( textual ), toText )

-- dhall -------------------------------

import qualified  Dhall.Core  as  DC

import Dhall  ( Interpret( autoWith ), Type( Type, expected, extract ) )

-- parsec ------------------------------

import qualified Text.Parsec.Char        as  TC

-- parsers -----------------------------

import qualified  Text.Parser.Char        as  RC

-- QuickCheck --------------------------

import Test.QuickCheck.Arbitrary  ( Arbitrary( arbitrary ) )

-- scientific --------------------------

import Data.Scientific  ( floatingOrInteger )

-- template-haskell --------------------

import Language.Haskell.TH        ( appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Fluffy.Applicative  ( (⋪), (⊵) )
import Fluffy.Functor2     ( (⊳) )
import Fluffy.Parsec2      ( Parsecable( parser ), __parsecN )
import Fluffy.Quasi        ( mkQuasiQuoterExp )
import Fluffy.Textual      ( parseTextM )

--------------------------------------------------------------------------------

data MACAddress = MACAddress Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Eq, Ord, Show)

----------------------------------------

instance Printable MACAddress where
  print (MACAddress a b c d e f) = P.text $ [fmt|%02x-%02x-%02x-%02x-%02x-%02x|]
                                                    a    b    c    d    e    f

----------------------------------------

instance Textual MACAddress where
  textual = let hexByte = (\ (a,b) → a*16+b) ⊳ ((,) ⊳ TI.hexDigit ⊵ TI.hexDigit)
             in MACAddress ⊳ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte ⋪ (RC.oneOf ":-")
                           ⊵ hexByte

----------------------------------------

{- | Read a Word8, encoded as two 'hex' chars (a-f,A-F,0-9).  Use of any other
     characters will lead to an undefined result -}
hexRead ∷ Char → Char → Word8
hexRead a b = fst ∘ head $ readHex [a,b]

parseJSONHexByte ∷ Value → Parser Word8
parseJSONHexByte (String t) =
  case unpack t of
    []      → fail "empty hexbyte"
    s@[c]   → if isHexDigit c
              then return $ hexRead '0' c
              else fail $ [fmt|bad hex char '%s'|] s
    s@[a,b] → if all isHexDigit s
              then return $ hexRead a b
              else fail $ [fmt|bad hex char '%s'|] s
    _       → fail $ [fmt|hexbyte too long '%t'|] t
parseJSONHexByte (Number n) =
  case floatingOrInteger n of
    Left (f ∷ Double) → fail $ [fmt|floating hexbyte: %w|] f
    Right i | i < 0     → fail $ [fmt|negative hexbyte: %d|] i
            | i > 255   → fail $ [fmt|hexbyte > 255: %d|] i
            | otherwise → return (fromInteger i)

parseJSONHexByte invalid = typeMismatch "hexbyte" invalid

instance FromJSON MACAddress where
  parseJSON (String t) = parseTextM "MACAddress" t
  parseJSON (Array (toList → [a,b,c,d,e,f])) = MACAddress ⊳ parseJSONHexByte a
                                                          ⊵ parseJSONHexByte b
                                                          ⊵ parseJSONHexByte c
                                                          ⊵ parseJSONHexByte d
                                                          ⊵ parseJSONHexByte e
                                                          ⊵ parseJSONHexByte f
  parseJSON (Array xs) = fail $ [fmt|MACAddress must have 6 elements: %w|] xs
  parseJSON invalid    = typeMismatch "MACAddress" invalid

----------------------------------------

instance ToJSON MACAddress where
  toJSON = String ∘ toText

----------------------------------------

instance Parsecable MACAddress where
  parser = let hexByte = hexRead ⊳ TC.hexDigit ⊵ TC.hexDigit
            in MACAddress ⊳ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte ⋪ TC.oneOf ":-"
                          ⊵ hexByte

----------------------------------------

__macAddress ∷ Text → MACAddress
__macAddress = __parsecN

{-| quasi-quoter for MACAddresses -}
macAddress ∷ QuasiQuoter
macAddress =
  mkQuasiQuoterExp "MACAddress"
                   (\ t → appE (varE '__macAddress) (litE $ stringL t))

mac ∷ QuasiQuoter
mac = macAddress

instance Interpret MACAddress where
  autoWith _ = Type {..}
               where extract (DC.TextLit (DC.Chunks [] t)) = pure $ __parsecN t
                     extract _                             = empty
                     expected = DC.Text

instance Arbitrary MACAddress where
  arbitrary = MACAddress ⊳ arbitrary ⊵ arbitrary ⊵ arbitrary
                         ⊵ arbitrary ⊵ arbitrary ⊵ arbitrary

-- that's all, folks! ----------------------------------------------------------
