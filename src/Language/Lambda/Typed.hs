{-# LANGUAGE PatternSynonyms #-}
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
  }
  deriving (Eq, Show)

type NormalTerm = NormalTerm' Atom
type RigidTerm = NormalTerm' Variable
type FlexibleTerm = NormalTerm' Metavariable

termKind :: NormalTerm -> Either FlexibleTerm RigidTerm
termKind (NormalTerm (Heading binder' (AVar head')) arguments') =
  Right (NormalTerm (Heading binder' head') arguments')
termKind (NormalTerm (Heading binder' (AMetavar head')) arguments') =
  Left (NormalTerm (Heading binder' head') arguments')

pattern Rigid :: RigidTerm -> NormalTerm
pattern Rigid rigid <- (termKind -> Right rigid)
  where
    Rigid (NormalTerm (Heading binder' head') arguments') =
      NormalTerm (Heading binder' (AVar head')) arguments'

pattern Flexible :: FlexibleTerm -> NormalTerm
pattern Flexible flexible <- (termKind -> Left flexible)
  where
    Flexible (NormalTerm (Heading binder' head') arguments') =
      NormalTerm (Heading binder' (AMetavar head')) arguments'

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> z = Variable "z"
-- >>> t = Base "t"
-- >>> u = Base "u"
--
-- >>> asNormalTerm (Var x)
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = []})
-- >>> asNormalTerm (Application (Var x) (Var y))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = []}]})
-- >>> asNormalTerm (Application (Application (Var x) (Var y)) (Var z))
-- Just (NormalTerm {heading = Heading {binder = [], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = []},NormalTerm {heading = Heading {binder = [], head = AVar (Variable "z")}, arguments = []}]})
-- >>> asNormalTerm (Lambda z t (Var x))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = []})
-- >>> asNormalTerm (Lambda y t (Lambda z t (Var x)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "y",Base "t"),(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = []})
-- >>> asNormalTerm (Lambda z t (Application (Var x) (Var y)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = AVar (Variable "x")}, arguments = [NormalTerm {heading = Heading {binder = [], head = AVar (Variable "y")}, arguments = []}]})
-- >>> asNormalTerm (Application (Lambda x t (Var x)) (Var y))
-- Nothing
-- >>> asNormalTerm (Lambda x t (Application (Lambda z t (Var z)) (Var y)))
-- Nothing
asNormalTerm :: Expression -> Maybe NormalTerm
asNormalTerm (Lambda variable typ body) = do
  NormalTerm (Heading binder' head') arguments' <- asNormalTerm body
  Just (NormalTerm (Heading ((variable, typ) : binder') head') arguments')
asNormalTerm other = extractBody other
 where
  extractBody Lambda{} = Nothing
  extractBody (Application function argument) = do
    NormalTerm heading' arguments' <- extractBody function
    argument' <- asNormalTerm argument
    Just (NormalTerm heading' (arguments' <> [argument']))
  extractBody (Atom head') = Just (NormalTerm (Heading [] head') [])

scopedArguments :: NormalTerm' head -> [NormalTerm]
scopedArguments (NormalTerm (Heading scope _) arguments') =
  scopedArgument <$> arguments'
 where
  scopedArgument (NormalTerm (Heading binder' head') subarguments) =
    NormalTerm (Heading (scope <> binder') head') subarguments

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
-- >>> var v = NormalTerm (Heading [] (AVar v)) []
-- >>> meta v = NormalTerm (Heading [] (AMetavar v)) []
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x, var u], var c]
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y, var v], NormalTerm (Heading [] (AMetavar f)) [var c]]
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [], NormalTerm (Heading [(v, t)] (AMetavar y)) []), (NormalTerm (Heading [] (AMetavar f)) [var c], var c)])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(u, t)] (AVar b)) [meta x, var u]]
-- >>> right = NormalTerm (Heading [] (AVar a)) [NormalTerm (Heading [(v, t)] (AVar b)) [meta y, var v]]
-- >>> simplified = simplify (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] (AMetavar x)) [], NormalTerm (Heading [(v, t)] (AMetavar y)) [])])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [(u, t), (v, t)] (AVar a)) [var u, NormalTerm (Heading [(w, t)] (AVar v)) []]
-- >>> right = NormalTerm (Heading [(v, t), (w, t)] (AVar a)) [var v, NormalTerm (Heading [(u, t)] (AVar v)) []]
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
