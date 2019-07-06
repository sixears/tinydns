{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module DomainNames.Domain
  ( Domain( (<:>), domainHead, prepend )
  , DomainLabel, DomainLabels, IsDomainLabels( domainLabels, dLabels )
  , dLabel, domainLabel, parseDomainLabel, parseDomainLabel'
  , parseDomainLabels'
  )
where

-- base --------------------------------

import Control.Monad       ( mapM, return )
import Data.Bool           ( not )
import Data.Char           ( isAlphaNum )
import Data.Either         ( Either( Left, Right ) )
import Data.Eq             ( Eq )
import Data.Foldable       ( toList )
import Data.Function       ( ($), id )
import Data.Functor        ( fmap )
import Data.List.NonEmpty  ( NonEmpty( (:|) ), fromList, head )
import Data.Maybe          ( Maybe( Just, Nothing ) )
import Data.Ord            ( Ord, (>) )
import Data.String         ( String )
import Text.Show           ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∨) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( Interpret( autoWith ) )

-- fluffy ------------------------------

import Fluffy.Either    ( __right )
import Fluffy.Foldable  ( length )
import Fluffy.Functor   ( (⊳) )
import Fluffy.List      ( (⋮) )
import Fluffy.Quasi     ( mkQuasiQuoterExp )
import Fluffy.Text      ( splitOn )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( Iso', from, iso )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- template-haskell --------------------

import Language.Haskell.TH        ( ExpQ, appE, litE, stringL, varE )
import Language.Haskell.TH.Quote  ( QuasiQuoter )

-- text --------------------------------

import Data.Text  ( Text, any, intercalate, uncons )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import DomainNames.Constants          ( maxDomainLength, maxLabelLength )
import DomainNames.Error.DomainError  ( AsDomainError
                                      , DomainError( DomainEmptyErr
                                                   , DomainLengthErr )
                                      , throwAsDomainError
                                      )

import DomainNames.Error.DomainLabelError
  ( AsDomainLabelError, DomainLabelError( DomainLabelEmptyErr
                                        , DomainLabelHyphenFirstCharErr
                                        , DomainLabelIllegalCharErr
                                        , DomainLabelLengthErr
                                        )
  , throwAsDomainLabelError
  )

--------------------------------------------------------------------------------

{- | Glossary of Terms:

     * DomainLabel - An identifier, which may contribute to a domain name /
                     hostname.  In the name `www.google.com`, each of `www`,
                     `google`, and `com` are `DomainNamePart`s.
     * Domain      - A non-empty list of domain labels.  A Domain maybe
                     fully-qualified (an FQDN) or unqualified.  FQDNs have the
                     root domain (called '') as the last element, and so are
                     written (and parsed) with a trailing '.'.
     * Hostname    - Any FQDN that may be linked to one or more IPv4 addresses
                     (even if those addresses keep changing, e.g., via
                     round-robin DNS).  E.g., `www.google.com`.
     * LocalName   - The left-most DomainLabel of a Host FQDN (the `head` of the
                     list).  Thus, for the FQDN `www.google.com`; the HostName
                     is `www`.
 -}

------------------------------------------------------------

newtype DomainLabel = DomainLabel Text
  deriving (Eq, Hashable, NFData, Ord, Show)

instance Printable DomainLabel where
  print (DomainLabel dl) = P.text dl

instance Interpret DomainLabel where
  autoWith _ = __parseDomainLabel ⊳ D.strictText

------------------------------------------------------------

parseDomainLabel ∷ (Printable ρ, AsDomainLabelError ε, MonadError ε η) ⇒
                   ρ → η DomainLabel
parseDomainLabel (toText → d) =
  case uncons d of
    Nothing       → throwAsDomainLabelError DomainLabelEmptyErr
    Just ('-', _) → throwAsDomainLabelError $ DomainLabelHyphenFirstCharErr d
    _             → if any ( \ c → not $ isAlphaNum c ∨ c ≡ '-' ) d
                    then throwAsDomainLabelError $ DomainLabelIllegalCharErr d
                    else if length d > maxLabelLength
                         then throwAsDomainLabelError $ DomainLabelLengthErr d
                         else return $ DomainLabel d

parseDomainLabel' ∷ (Printable ρ, MonadError DomainLabelError η) ⇒
                    ρ → η DomainLabel
parseDomainLabel' = parseDomainLabel

__parseDomainLabel ∷ Printable ρ ⇒ ρ → DomainLabel
__parseDomainLabel = __right ∘ parseDomainLabel'

__parseDomainLabel' ∷ Text → DomainLabel
__parseDomainLabel' = __parseDomainLabel

domainLabel ∷ QuasiQuoter
domainLabel = let parseExp ∷ String → ExpQ
                  parseExp = appE (varE '__parseDomainLabel') ∘ litE ∘ stringL
               in mkQuasiQuoterExp "domainLabel" parseExp

dLabel ∷ QuasiQuoter
dLabel = domainLabel
-- dl ∷ QuasiQuoter
-- dl = domainLabel

------------------------------------------------------------

class IsDomainLabels δ where
  domainLabels ∷ Iso' δ DomainLabels
  dLabels      ∷ Iso' δ (NonEmpty DomainLabel)
  dLabels      = iso (view dLabels ∘ view domainLabels)
                     (view (from domainLabels) ∘ view (from dLabels))

newtype DomainLabels = DomainLabels { unDomainLabels ∷ NonEmpty DomainLabel }
  deriving (Eq, Hashable, NFData, Ord, Show)

instance IsDomainLabels DomainLabels where
  domainLabels = id
  dLabels = iso unDomainLabels DomainLabels

{- | Render a domain, without a trailing dot even for FQDN, to make the 253-char
     check that excludes any trailing dot
-}
renderDomainLabels ∷ NonEmpty DomainLabel → Text
renderDomainLabels ds = Data.Text.intercalate "." (toText ⊳ toList ds)

instance Printable DomainLabels where
  print (DomainLabels dls) = P.text $ renderDomainLabels dls

instance Interpret DomainLabels where
  autoWith iopts = DomainLabels ⊳ fmap fromList (D.list (autoWith iopts))

----------------------------------------

checkDomainLength ∷ (AsDomainError ε, MonadError ε η) ⇒
                    NonEmpty DomainLabel → η DomainLabels
checkDomainLength dls = let txt = renderDomainLabels dls
                         in if length txt > maxDomainLength
                            then throwAsDomainError $ DomainLengthErr txt
                            else return $ DomainLabels dls

----------------------------------------

parseDomainLabels ∷ (Printable ρ, AsDomainError ε, MonadError ε η) ⇒
                    ρ → η DomainLabels
parseDomainLabels (splitOn "." ∘ toText → ("" :| [])) =
  throwAsDomainError DomainEmptyErr
parseDomainLabels (splitOn "." ∘ toText → ds) =
  case mapM parseDomainLabel' ds of
    Left  dle → throwAsDomainError dle
    Right ds' → checkDomainLength ds'
  
parseDomainLabels' ∷ (Printable ρ, MonadError DomainError η) ⇒
                     ρ → η DomainLabels
parseDomainLabels' = parseDomainLabels

------------------------------------------------------------

class IsDomainLabels δ ⇒ Domain δ where
--  domainLabels ∷ δ → DomainLabels
  prepend ∷ (AsDomainError ε, MonadError ε η) ⇒ DomainLabel → δ → η δ
  domainHead ∷ δ → DomainLabel

  (<:>) ∷ MonadError DomainError η ⇒ DomainLabel → δ → η δ
  (<:>) = prepend

instance Domain DomainLabels where
  prepend d  (DomainLabels ds) = checkDomainLength (d ⋮ ds)
  domainHead (DomainLabels ds) = head ds

-- that's all, folks! ----------------------------------------------------------
