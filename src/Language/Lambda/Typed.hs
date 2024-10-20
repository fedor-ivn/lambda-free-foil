{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Lambda.Typed (
  Type (..),
  Variable (..),
  Metavariable (..),
  Atom (..),
  Expression (..),
  pattern Var,
  pattern Metavar,
  typeOf,
  Heading (..),
  NormalTerm' (..),
  NormalTerm,
  RigidTerm,
  FlexibleTerm,
  pattern Rigid,
  pattern Flexible,
  asNormalTerm,
  DisagreementSet (..),
  Node (..),
  simplify,
) where

import Prelude hiding (head)

data Type
  = Base String
  | Function Type Type
  deriving (Eq, Show)

newtype Variable = Variable String deriving (Eq, Show)

newtype Metavariable = Metavariable String deriving (Eq, Show)

data Atom
  = AVar Variable
  | AMetavar Metavariable
  deriving (Eq, Show)

data Expression
  = Atom Atom
  | Application Expression Expression
  | Lambda Variable Type Expression
  deriving (Eq, Show)

pattern Var :: Variable -> Expression
pattern Var variable = Atom (AVar variable)

pattern Metavar :: Metavariable -> Expression
pattern Metavar variable = Atom (AMetavar variable)

-- >>> t = Base "t"
-- >>> u = Base "u"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
--
-- >>> typeOf (const (Just t)) (Var x)
-- Just (Base "t")
-- >>> typeOf (const Nothing) (Lambda x t (Var x))
-- Just (Function (Base "t") (Base "t"))
-- >>> typeOf (const (Just u)) (Lambda x t (Var y))
-- Just (Function (Base "t") (Base "u"))
-- >>> typeOf (const (Just t)) (Application (Lambda x t (Var x)) (Var x))
-- Just (Base "t")
-- >>> typeOf (const (Just u)) (Application (Lambda x t (Var x)) (Var x))
-- Nothing
typeOf :: (Atom -> Maybe Type) -> Expression -> Maybe Type
typeOf typeOfAtom (Atom atom) = typeOfAtom atom
typeOf typeOfAtom (Application function argument) = do
  functionType <- typeOf typeOfAtom function
  argumentType <- typeOf typeOfAtom argument
  case functionType of
    Function argumentType' returnType
      | argumentType == argumentType' -> Just returnType
    _ -> Nothing
typeOf typeOfAtom (Lambda boundVariable typ body) = do
  returnType <- flip typeOf body $ \variable ->
    if variable == AVar boundVariable
      then Just typ
      else typeOfAtom variable
  Just (Function typ returnType)

data Heading head = Heading
  { binder :: [(Variable, Type)]
  , head :: head
  }
  deriving (Eq, Show)

-- Ignores types of parameters (at least for now).
--
-- >>> a = Variable "a"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
--
-- >>> areAlphaEquivalent (Heading [(x, t)] x) (Heading [(y, t)] y)
-- True
-- >>> areAlphaEquivalent (Heading [(x, t)] a) (Heading [(y, t)] a)
-- True
-- >>> areAlphaEquivalent (Heading [(x, t)] x) (Heading [(y, t)] x)
-- False
-- >>> areAlphaEquivalent (Heading [(y, t)] x) (Heading [(x, t)] x)
-- False
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] x) (Heading [(x, t)] x)
-- False
areAlphaEquivalent :: Heading Variable -> Heading Variable -> Bool
areAlphaEquivalent (Heading [] leftHead) (Heading [] rightHead) =
  leftHead == rightHead
areAlphaEquivalent
  (Heading ((leftVariable, _) : leftBinder) leftHead)
  (Heading ((rightVariable, _) : rightBinder) rightHead)
    | leftVariable == leftHead =
        rightVariable == rightHead && length leftBinder == length rightBinder
    | rightVariable == rightHead = False
    | otherwise =
        areAlphaEquivalent
          (Heading leftBinder leftHead)
          (Heading rightBinder rightHead)
areAlphaEquivalent _ _ = False

data NormalTerm' head = NormalTerm
  { heading :: Heading head
  , arguments :: [NormalTerm]
  , returnType :: Type
  }
  deriving (Eq, Show)

type NormalTerm = NormalTerm' Atom
type RigidTerm = NormalTerm' Variable
type FlexibleTerm = NormalTerm' Metavariable

termKind :: NormalTerm -> Either FlexibleTerm RigidTerm
termKind NormalTerm{heading = Heading binder (AVar head), ..} =
  Right (NormalTerm{heading = Heading binder head, ..})
termKind NormalTerm{heading = Heading binder (AMetavar head), ..} =
  Left (NormalTerm{heading = Heading binder head, ..})

pattern Rigid :: RigidTerm -> NormalTerm
pattern Rigid rigid <- (termKind -> Right rigid)
  where
    Rigid NormalTerm{heading = Heading binder head, ..} =
      NormalTerm{heading = Heading binder (AVar head), ..}

pattern Flexible :: FlexibleTerm -> NormalTerm
pattern Flexible flexible <- (termKind -> Left flexible)
  where
    Flexible NormalTerm{heading = Heading binder head, ..} =
      NormalTerm{heading = Heading binder (AMetavar head), ..}

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> z = Variable "z"
-- >>> t = Base "t"
-- >>> typeOfAtom var = if var == AVar x then Just (Function t (Function t t)) else Just t
--
-- >>> asNormalTerm typeOfAtom (Var x)
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Application (Var x) (Var y))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"}], returnType = Function (Base "t") (Base "t")})
--
-- >>> asNormalTerm typeOfAtom (Application (Application (Var x) (Var y)) (Var z))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "z")}, arguments = [], returnType = Base "t"}], returnType = Base "t"})
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Var x))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Lambda y t (Lambda z t (Var x)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "y",Base "t"),(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [], returnType = Function (Base "t") (Function (Base "t") (Base "t"))})
--
-- >>> asNormalTerm typeOfAtom (Lambda z t (Application (Var x) (Var y)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = [], returnType = Base "t"}], returnType = Function (Base "t") (Base "t")})
--
-- >>> asNormalTerm typeOfAtom (Application (Lambda x t (Var x)) (Var y))
-- Nothing
--
-- >>> asNormalTerm typeOfAtom (Lambda x t (Application (Lambda z t (Var z)) (Var y)))
-- Nothing
asNormalTerm :: (Atom -> Maybe Type) -> Expression -> Maybe NormalTerm
asNormalTerm typeOfAtom (Lambda variable typ body) = do
  let typeOfAtom' var
        | var == AVar variable = Just typ
        | otherwise = typeOfAtom var
  NormalTerm{heading = (Heading binder head), ..} <- asNormalTerm typeOfAtom' body
  Just (NormalTerm{heading = Heading ((variable, typ) : binder) head, ..})
asNormalTerm typeOfAtom other = do
  returnType <- typeOf typeOfAtom other
  (heading, arguments) <- extractBody other
  return (NormalTerm heading arguments returnType)
 where
  extractBody Lambda{} = Nothing
  extractBody (Application function argument) = do
    (heading, arguments) <- extractBody function
    normalArgument <- asNormalTerm typeOfAtom argument
    Just (heading, arguments <> [normalArgument])
  extractBody (Atom head) = Just (Heading [] head, [])

scopedArguments :: NormalTerm' head -> [NormalTerm]
scopedArguments (NormalTerm (Heading scope _) arguments _) =
  scopedArgument <$> arguments
 where
  scopedArgument (NormalTerm (Heading binder head) subarguments returnType) =
    NormalTerm (Heading (scope <> binder) head) subarguments returnType

newtype DisagreementSet = DisagreementSet [(NormalTerm, NormalTerm)]
  deriving (Eq, Show)

data Node
  = Failure
  | Nonterminal DisagreementSet
  | Success
  deriving (Eq, Show)

-- Does not return 'Success' -- it's better be done when searching for a pair
-- with a rigid term.
--
-- >>> a = Variable "a"
-- >>> b = Variable "b"
-- >>> c = Variable "c"
-- >>> f = Metavariable "F"
-- >>> u = Variable "u"
-- >>> v = Variable "v"
-- >>> w = Variable "w"
-- >>> x = Metavariable "X"
-- >>> y = Metavariable "Y"
-- >>> t = Base "t"
-- >>> var v t = NormalTerm (Heading [] (AVar v)) [] t
-- >>> meta v t = NormalTerm (Heading [] (AMetavar v)) [] t
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x t, var u t] t, var c t] t
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y t, var v t] t, NormalTerm (Heading [] (AMetavar f)) [var c t] t] t
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [] t, NormalTerm (Heading [(v, t)] (AMetavar y)) [] t), (NormalTerm (Heading [] (AMetavar f)) [var c t] t, var c t)])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x t, var u t] t] t
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y t, var v t] t] t
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [] t, NormalTerm (Heading [(v, t)] (AMetavar y)) [] t)])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [(u, t), (v, t)] (AVar a)) [var u t, NormalTerm (Heading [(w, t)] (AVar v)) [] t] t
-- >>> right = NormalTerm (Heading [(v, t), (w, t)] (AVar a)) [var v t, NormalTerm (Heading [(u, t)] (AVar v)) [] t] t
-- >>> simplify (DisagreementSet [(left, right)])
-- Failure
simplify :: DisagreementSet -> Node
simplify (DisagreementSet set) = case simplifyRigidPairs set of
  Nothing -> Failure
  Just set' -> Nonterminal (DisagreementSet (putRigidOnRight <$> set'))
 where
  simplifyRigidPairs = foldr simplifyRigidPair (Just [])
  simplifyRigidPair (Rigid left, Rigid right) rest
    | areAlphaEquivalent (heading left) (heading right) = do
        newPairs <- simplifyRigidPairs (zip (scopedArguments left) (scopedArguments right))
        rest' <- rest
        return (newPairs <> rest')
    | otherwise = Nothing
  simplifyRigidPair pair rest = (pair :) <$> rest

  -- TODO: is this really needed here?
  putRigidOnRight (left@(Rigid _), right) = (right, left)
  putRigidOnRight (left, right) = (left, right)
