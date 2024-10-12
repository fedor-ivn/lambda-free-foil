module Language.Lambda.Typed (
  Type (..),
  Variable (..),
  Expression (..),
  typeOf,
  Heading (..),
  isRigid,
  NormalTerm (..),
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

data Expression
  = Var Variable
  | Application Expression Expression
  | Lambda Variable Type Expression
  deriving (Eq, Show)

-- >>> t = Base "t"
-- >>> u = Base "u"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
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
typeOf :: (Variable -> Maybe Type) -> Expression -> Maybe Type
typeOf typeOfVariable (Var variable) = typeOfVariable variable
typeOf typeOfVariable (Application function argument) = do
  functionType <- typeOf typeOfVariable function
  argumentType <- typeOf typeOfVariable argument
  case functionType of
    Function argumentType' returnType
      | argumentType == argumentType' -> Just returnType
    _ -> Nothing
typeOf typeOfVariable (Lambda boundVariable typ body) = do
  returnType <- flip typeOf body $ \variable ->
    if variable == boundVariable
      then Just typ
      else typeOfVariable variable
  Just (Function typ returnType)

data Heading = Heading
  { binder :: [(Variable, Type)]
  , head :: Variable
  }
  deriving (Eq, Show)

-- >>> _C = Variable "C"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
--
-- >>> isRigid (== _C) (Heading [(x, t)] x)
-- True
-- >>> isRigid (== _C) (Heading [(x, t)] _C)
-- True
-- >>> isRigid (== _C) (Heading [(x, t)] y)
-- False
isRigid :: (Variable -> Bool) -> Heading -> Bool
isRigid isConstant (Heading binder' head') =
  head' `elem` (fst <$> binder') || isConstant head'

-- Ignores types of parameters (at least for now).
--
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> a = Variable "a"
-- >>> b = Variable "b"
-- >>> q = Variable "q"
-- >>> t = Base "t"
--
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] x) (Heading [(a, t), (b, t)] a)
-- True
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] x) (Heading [(b, t), (a, t)] a)
-- False
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] x) (Heading [(b, t), (a, t)] x)
-- False
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] a) (Heading [(b, t), (a, t)] a)
-- False
-- >>> areAlphaEquivalent (Heading [(x, t), (y, t)] q) (Heading [(b, t), (a, t)] q)
-- True
areAlphaEquivalent :: Heading -> Heading -> Bool
areAlphaEquivalent (Heading [] leftHead) (Heading [] rightHead) =
  leftHead == rightHead
areAlphaEquivalent
  (Heading ((leftVariable, _) : leftBinder) leftHead)
  (Heading ((rightVariable, _) : rightBinder) rightHead)
    | leftVariable == leftHead =
        rightVariable == rightHead && length leftBinder == length rightBinder
    | rightVariable == rightHead = False
    | otherwise =
        areAlphaEquivalent (Heading leftBinder leftHead) (Heading rightBinder rightHead)
areAlphaEquivalent _ _ = False

data NormalTerm = NormalTerm
  { heading :: Heading
  , arguments :: [NormalTerm]
  }
  deriving (Eq, Show)

-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> z = Variable "z"
-- >>> t = Base "t"
-- >>> u = Base "u"
--
-- >>> asNormalTerm (Var x)
-- Just (NormalTerm {heading = Heading {binder = [], head = Variable "x"}, arguments = []})
-- >>> asNormalTerm (Application (Var x) (Var y))
-- Just (NormalTerm {heading = Heading {binder = [], head = Variable "x"}, arguments = [NormalTerm {heading = Heading {binder = [], head = Variable "y"}, arguments = []}]})
-- >>> asNormalTerm (Application (Application (Var x) (Var y)) (Var z))
-- Just (NormalTerm {heading = Heading {binder = [], head = Variable "x"}, arguments = [NormalTerm {heading = Heading {binder = [], head = Variable "y"}, arguments = []},NormalTerm {heading = Heading {binder = [], head = Variable "z"}, arguments = []}]})
-- >>> asNormalTerm (Lambda z t (Var x))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = Variable "x"}, arguments = []})
-- >>> asNormalTerm (Lambda y t (Lambda z t (Var x)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "y",Base "t"),(Variable "z",Base "t")], head = Variable "x"}, arguments = []})
-- >>> asNormalTerm (Lambda z t (Application (Var x) (Var y)))
-- Just (NormalTerm {heading = Heading {binder = [(Variable "z",Base "t")], head = Variable "x"}, arguments = [NormalTerm {heading = Heading {binder = [], head = Variable "y"}, arguments = []}]})
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
  extractBody (Var head') = Just (NormalTerm (Heading [] head') [])

scopedArguments :: NormalTerm -> [NormalTerm]
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
-- >>> f = Variable "f"
-- >>> u = Variable "u"
-- >>> v = Variable "v"
-- >>> w = Variable "w"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> t = Base "t"
-- >>> isConstant = (`elem` [a, b, c])
-- >>> var v = NormalTerm (Heading [] v) []
--
-- >>> left = NormalTerm (Heading [] a) [NormalTerm (Heading [(u, t)] b) [var x, var u], var c]
-- >>> right = NormalTerm (Heading [] a) [NormalTerm (Heading [(v, t)] b) [var y, var v], NormalTerm (Heading [] f) [var c]]
-- >>> simplified = simplify isConstant (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] x) [], NormalTerm (Heading [(v, t)] y) []), (NormalTerm (Heading [] f) [var c], var c)])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [] a) [NormalTerm (Heading [(u, t)] b) [var x, var u]]
-- >>> right = NormalTerm (Heading [] a) [NormalTerm (Heading [(v, t)] b) [var y, var v]]
-- >>> simplified = simplify isConstant (DisagreementSet [(left, right)])
-- >>> expected = Nonterminal (DisagreementSet [(NormalTerm (Heading [(u, t)] x) [], NormalTerm (Heading [(v, t)] y) [])])
-- >>> simplified == expected
-- True
--
-- >>> left = NormalTerm (Heading [(u, t), (v, t)] a) [var y, NormalTerm (Heading [(w, t)] v) []]
-- >>> right = NormalTerm (Heading [(v, t), (w, t)] a) [var v, NormalTerm (Heading [(u, t)] b) []]
-- >>> simplify isConstant (DisagreementSet [(left, right)])
-- Failure
simplify :: (Variable -> Bool) -> DisagreementSet -> Node
simplify isConstant (DisagreementSet set) = case simplifyRigidPairs set of
  Nothing -> Failure
  Just set' -> Nonterminal (DisagreementSet (putRigidOnRight <$> set'))
 where
  simplifyRigidPairs = foldr simplifyRigidPair (Just [])
  simplifyRigidPair pair@(left, right) rest
    | not (isRigid isConstant (heading left)) = (pair :) <$> rest
    | not (isRigid isConstant (heading right)) = (pair :) <$> rest
    | not (areAlphaEquivalent (heading left) (heading right)) = Nothing
    | otherwise = do
        newPairs <- simplifyRigidPairs (zip (scopedArguments left) (scopedArguments right))
        rest' <- rest
        return (newPairs <> rest')

  -- TODO: is this really needed here?
  putRigidOnRight (left, right)
    | isRigid isConstant (heading left) = (right, left)
    | otherwise = (left, right)
