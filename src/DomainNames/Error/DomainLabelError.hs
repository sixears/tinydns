{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module DomainNames.Error.DomainLabelError
  ( AsDomainLabelError( _DomainLabelError )
  , DomainLabelError( DomainLabelEmptyErr, DomainLabelHyphenFirstCharErr
                    , DomainLabelIllegalCharErr, DomainLabelLengthErr )
  , throwAsDomainLabelError
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fluffy ------------------------------

import Fluffy.Foldable   ( length )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Constants  ( maxLabelLength )

--------------------------------------------------------------------------------

data DomainLabelError = DomainLabelEmptyErr
                      | DomainLabelHyphenFirstCharErr Text
                      | DomainLabelLengthErr     Text
                      | DomainLabelIllegalCharErr     Text
  deriving (Eq, Show)

instance Exception DomainLabelError

instance Printable DomainLabelError where
  print DomainLabelEmptyErr = P.text "empty domain label"
  print (DomainLabelHyphenFirstCharErr d) = P.text $ 
    [fmt|domain label first character must not be a hyphen '%t'|] d
  print (DomainLabelLengthErr d) = P.text $ 
    [fmt|domain label length %d exceeds %d '%t'|] (length d) maxLabelLength d
  print (DomainLabelIllegalCharErr d) = P.text $ 
    [fmt|domain label characters must be alpha-numeric or hyphen '%t'|] d

--------------------

class AsDomainLabelError ε where
  _DomainLabelError ∷ Prism' ε DomainLabelError

instance AsDomainLabelError DomainLabelError where
  _DomainLabelError = id

throwAsDomainLabelError ∷ (AsDomainLabelError ε, MonadError ε η) ⇒
                          DomainLabelError → η α
throwAsDomainLabelError = throwError ∘ (_DomainLabelError #)
    
-- that's all, folks! ----------------------------------------------------------
