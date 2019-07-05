{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Fluffy.Dhall.ErrorOrphans
  ( )
where

-- base --------------------------------

import Data.Bool          ( Bool( False, True ) )
import Data.Eq            ( Eq( (==) ) )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )

-- dhall -------------------------------

import Dhall.Context    ( Context, toList )
import Dhall.TypeCheck  ( TypeMessage( AnnotMismatch
                                     , CantAccess
                                     , CantAdd
                                     , CantAnd
                                     , CantEQ
                                     , CantInterpolate
                                     , CantListAppend
                                     , CantMultiply
                                     , CantNE
                                     , CantOr
                                     , CantProject
                                     , CantTextAppend
                                     , CombineTypesRequiresRecordType
                                     , ConstructorsRequiresAUnionType
                                     , DuplicateAlternative
                                     , FieldAnnotationMismatch
                                     , FieldCollision
                                     , FieldMismatch
                                     , HandlerInputTypeMismatch
                                     , HandlerNotAFunction
                                     , HandlerOutputTypeMismatch
                                     , IfBranchMismatch
                                     , IfBranchMustBeTerm
                                     , InvalidAlternative
                                     , InvalidAlternativeType
                                     , InvalidField
                                     , InvalidFieldType
                                     , InvalidHandlerOutputType
                                     , InvalidInputType
                                     , InvalidListElement
                                     , InvalidListType
                                     , InvalidOptionalElement
                                     , InvalidOptionalType
                                     , InvalidOutputType
                                     , InvalidPredicate
                                     , InvalidSome
                                     , ListAppendMismatch
                                     , MismatchedListElements
                                     , MissingField
                                     , MissingHandler
                                     , MissingListType
                                     , MissingMergeType
                                     , MustCombineARecord
                                     , MustMergeARecord
                                     , MustMergeUnion
                                     , NoDependentTypes
                                     , NotAFunction
                                     , RecordMismatch
                                     , RecordTypeMismatch
                                     , TypeMismatch
                                     , UnboundVariable
                                     , Untyped
                                     , UnusedHandler
                                     ) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

instance (Eq σ, Eq α) ⇒ Eq (TypeMessage σ α) where
  (UnboundVariable t) == (UnboundVariable t') = t ≡ t'
  (UnboundVariable _) == _ = False

  (InvalidInputType e) == (InvalidInputType e') = e ≡ e'
  (InvalidInputType _) == _ = False

  (InvalidOutputType e) == (InvalidOutputType e') = e ≡ e'
  (InvalidOutputType _) == _ = False

  (NotAFunction e0 e1) == (NotAFunction e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (NotAFunction _ _) == _ = False

  (TypeMismatch e0 e1 e2 e3) == (TypeMismatch e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (TypeMismatch _ _ _ _) == _ = False

  (AnnotMismatch e0 e1 e2) == (AnnotMismatch e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (AnnotMismatch _ _ _) == _ = False

  Untyped == Untyped = True
  Untyped == _ = False

  MissingListType == MissingListType = True
  MissingListType == _ = False

  (MismatchedListElements e0 e1 e2 e3) == (MismatchedListElements e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (MismatchedListElements _ _ _ _) == _ = False

  (InvalidListElement e0 e1 e2 e3) == (InvalidListElement e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (InvalidListElement _ _ _ _) == _ = False

  (InvalidListType e) == (InvalidListType e') = e ≡ e'
  (InvalidListType _) == _ = False

  (InvalidOptionalElement e0 e1 e2) == (InvalidOptionalElement e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (InvalidOptionalElement _ _ _) == _ = False

  (InvalidOptionalType e) == (InvalidOptionalType e') = e ≡ e'
  (InvalidOptionalType _) == _ = False

  (InvalidSome e0 e1 e2) == (InvalidSome e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (InvalidSome _ _ _) == _ = False

  (InvalidPredicate e0 e1) == (InvalidPredicate e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (InvalidPredicate _ _) == _ = False

  (IfBranchMismatch e0 e1 e2 e3) == (IfBranchMismatch e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (IfBranchMismatch _ _ _ _) == _ = False

  (IfBranchMustBeTerm e0 e1 e2 e3) == (IfBranchMustBeTerm e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (IfBranchMustBeTerm _ _ _ _) == _ = False

  (InvalidField e0 e1) == (InvalidField e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (InvalidField _ _) == _ = False

  (InvalidFieldType e0 e1) == (InvalidFieldType e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (InvalidFieldType _ _) == _ = False

  (FieldAnnotationMismatch e0 e1 e2 e3 e4 e5) == (FieldAnnotationMismatch e0' e1' e2' e3' e4' e5') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3' ∧ e4 ≡ e4' ∧ e5 ≡ e5'
  (FieldAnnotationMismatch _ _ _ _ _ _) == _ = False

  (FieldMismatch e0 e1 e2 e3 e4 e5) == (FieldMismatch e0' e1' e2' e3' e4' e5') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3' ∧ e4 ≡ e4' ∧ e5 ≡ e5'
  (FieldMismatch _ _ _ _ _ _) == _ = False

  (InvalidAlternative e0 e1) == (InvalidAlternative e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (InvalidAlternative _ _) == _ = False

  (InvalidAlternativeType e0 e1) == (InvalidAlternativeType e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (InvalidAlternativeType _ _) == _ = False

  (ListAppendMismatch e0 e1) == (ListAppendMismatch e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (ListAppendMismatch _ _) == _ = False

  (DuplicateAlternative e0) == (DuplicateAlternative e0') = e0 ≡ e0'
  (DuplicateAlternative _) == _ = False

  (MustCombineARecord e0 e1 e2) == (MustCombineARecord e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (MustCombineARecord _ _ _) == _ = False

  (RecordMismatch e0 e1 e2 e3 e4) == (RecordMismatch e0' e1' e2' e3' e4') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3' ∧ e4 ≡ e4'
  (RecordMismatch _ _ _ _ _) == _ = False

  (CombineTypesRequiresRecordType e0 e1) == (CombineTypesRequiresRecordType e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CombineTypesRequiresRecordType _ _) == _ = False

  (RecordTypeMismatch e0 e1 e2 e3) == (RecordTypeMismatch e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (RecordTypeMismatch _ _ _ _) == _ = False

  (FieldCollision e0) == (FieldCollision e0') = e0 ≡ e0'
  (FieldCollision _) == _ = False

  (MustMergeARecord e0 e1) == (MustMergeARecord e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (MustMergeARecord _ _) == _ = False

  (MustMergeUnion e0 e1) == (MustMergeUnion e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (MustMergeUnion _ _) == _ = False

  (UnusedHandler e0) == (UnusedHandler e0') = e0 ≡ e0'
  (UnusedHandler _) == _ = False

  (MissingHandler e0) == (MissingHandler e0') = e0 ≡ e0'
  (MissingHandler _) == _ = False

  (HandlerInputTypeMismatch e0 e1 e2) == (HandlerInputTypeMismatch e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (HandlerInputTypeMismatch _ _ _) == _ = False

  (HandlerOutputTypeMismatch e0 e1 e2 e3) == (HandlerOutputTypeMismatch e0' e1' e2' e3') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2' ∧ e3 ≡ e3'
  (HandlerOutputTypeMismatch _ _ _ _) == _ = False

  (InvalidHandlerOutputType e0 e1 e2) == (InvalidHandlerOutputType e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (InvalidHandlerOutputType _ _ _) == _ = False

  MissingMergeType == MissingMergeType = True
  MissingMergeType == _ = False

  (HandlerNotAFunction e0 e1) == (HandlerNotAFunction e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (HandlerNotAFunction _ _) == _ = False

  (ConstructorsRequiresAUnionType e0 e1) == (ConstructorsRequiresAUnionType e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (ConstructorsRequiresAUnionType _ _) == _ = False

  (CantAccess e0 e1 e2) == (CantAccess e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (CantAccess _ _ _) == _ = False

  (CantProject e0 e1 e2) == (CantProject e0' e1' e2') = e0 ≡ e0' ∧ e1 ≡ e1' ∧ e2 ≡ e2'
  (CantProject _ _ _) == _ = False

  (MissingField e0 e1) == (MissingField e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (MissingField _ _) == _ = False

  (CantAnd e0 e1) == (CantAnd e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantAnd _ _) == _ = False

  (CantOr e0 e1) == (CantOr e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantOr _ _) == _ = False

  (CantEQ e0 e1) == (CantEQ e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantEQ _ _) == _ = False

  (CantNE e0 e1) == (CantNE e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantNE _ _) == _ = False

  (CantInterpolate e0 e1) == (CantInterpolate e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantInterpolate _ _) == _ = False

  (CantTextAppend e0 e1) == (CantTextAppend e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantTextAppend _ _) == _ = False

  (CantListAppend e0 e1) == (CantListAppend e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantListAppend _ _) == _ = False

  (CantAdd e0 e1) == (CantAdd e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantAdd _ _) == _ = False

  (CantMultiply e0 e1) == (CantMultiply e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (CantMultiply _ _) == _ = False

  (NoDependentTypes e0 e1) == (NoDependentTypes e0' e1') = e0 ≡ e0' ∧ e1 ≡ e1'
  (NoDependentTypes _ _) == _ = False

instance Eq α ⇒ Eq (Context α) where
  c == c' = (toList c) ≡ (toList c')

-- that's all, folks! ----------------------------------------------------------
