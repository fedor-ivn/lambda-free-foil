module Language.Lambda.Typed (
  Type (..),
  Variable (..),
  Expression (..),
  typeOf,
  NormalTerm (..),
  asNormalTerm,
  isRigid,
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

data NormalTerm = NormalTerm
  { binder :: [(Variable, Type)]
  , head :: Variable
  , arguments :: [NormalTerm]
  }
  deriving (Eq, Show)

-- >>> t = Base "t"
-- >>> u = Base "u"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> z = Variable "z"
-- >>> asNormalTerm (Var x)
-- Just (NormalTerm {binder = [], head = Variable "x", arguments = []})
-- >>> asNormalTerm (Application (Var x) (Var y))
-- Just (NormalTerm {binder = [], head = Variable "x", arguments = [NormalTerm {binder = [], head = Variable "y", arguments = []}]})
-- >>> asNormalTerm (Application (Application (Var x) (Var y)) (Var z))
-- Just (NormalTerm {binder = [], head = Variable "x", arguments = [NormalTerm {binder = [], head = Variable "y", arguments = []},NormalTerm {binder = [], head = Variable "z", arguments = []}]})
-- >>> asNormalTerm (Lambda z t (Var x))
-- Just (NormalTerm {binder = [(Variable "z",Base "t")], head = Variable "x", arguments = []})
-- >>> asNormalTerm (Lambda y t (Lambda z t (Var x)))
-- Just (NormalTerm {binder = [(Variable "y",Base "t"),(Variable "z",Base "t")], head = Variable "x", arguments = []})
-- >>> asNormalTerm (Lambda z t (Application (Var x) (Var y)))
-- Just (NormalTerm {binder = [(Variable "z",Base "t")], head = Variable "x", arguments = [NormalTerm {binder = [], head = Variable "y", arguments = []}]})
-- >>> asNormalTerm (Application (Lambda x t (Var x)) (Var y))
-- Nothing
-- >>> asNormalTerm (Lambda x t (Application (Lambda z t (Var z)) (Var y)))
-- Nothing
asNormalTerm :: Expression -> Maybe NormalTerm
asNormalTerm (Lambda variable typ body) = do
  NormalTerm binder' head' arguments' <- asNormalTerm body
  Just (NormalTerm ((variable, typ) : binder') head' arguments')
asNormalTerm other = extractBody other
 where
  extractBody (Lambda{}) = Nothing
  extractBody (Application function argument) = do
    NormalTerm binder' head' arguments' <- extractBody function
    argument' <- asNormalTerm argument
    Just (NormalTerm binder' head' (arguments' <> [argument']))
  extractBody (Var head') = Just (NormalTerm [] head' [])

-- >>> import Data.Char (isUpper)
-- >>> _C = Variable "C"
-- >>> x = Variable "x"
-- >>> y = Variable "y"
-- >>> isRigid (== _C) (NormalTerm [(x, Base "t")] x [])
-- True
-- >>> isRigid (== _C) (NormalTerm [(x, Base "t")] _C [])
-- True
-- >>> isRigid (== _C) (NormalTerm [(x, Base "t")] y [])
-- False
isRigid :: (Variable -> Bool) -> NormalTerm -> Bool
isRigid isConstant (NormalTerm binder' head' _) =
  head' `elem` (fst <$> binder') || isConstant head'
