{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

-- | Free foil implementation of the \(\lambda)-calculus (with pairs).
--
-- Free foil provides __general__ definitions or implementations for the
-- following:
--
-- 1. Freely generated (from a simple signature) scope-safe AST.
-- 2. Correct capture-avoiding substitution (see 'substitute').
-- 3. Correct α-equivalence checks (see 'alphaEquiv' and 'alphaEquivRefreshed')
--    as well as α-normalization (see 'refreshAST').
-- 4. Conversion helpers (see 'convertToAST' and 'convertFromAST').
--
-- The following is __generated__ using Template Haskell:
--
-- 1. Convenient pattern synonyms.
-- 2. 'ZipMatch' instances (enabling general α-equivalence).
-- 3. Conversion between scope-safe and raw term representation.
--
-- The following is implemented __manually__ in this module:
--
-- 1. Computation of weak head normal form (WHNF), see 'whnf'.
-- 2. Entry point, gluing everything together. See 'defaultMain'.
--
-- __Note:__ free foil does not (easily) support patterns at the moment, so only
-- wildcard patterns and variable patterns are handled in this implementation.
module Language.Lambda.Impl where

import qualified Control.Monad.Foil as Foil
import Control.Monad.Foil.Internal as FoilInternal
import Control.Monad.Foil.TH
import Control.Monad.Free.Foil
import Control.Monad.Free.Foil.Generic
import Control.Monad.Free.Foil.TH
import Data.Biapplicative (Bifunctor (bimap))
import Data.Bifunctor.Sum
import Data.Bifunctor.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (..))
import qualified GHC.Generics as GHC
import Generics.Kind.TH (deriveGenericK)
import qualified Language.Lambda.Syntax.Abs as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Par as Raw
import qualified Language.Lambda.Syntax.Print as Raw
import System.Exit (exitFailure)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified Control.Monad.Foil as Foil
-- >>> import Control.Monad.Free.Foil
-- >>> import Data.String (fromString)

-- * Generated code

-- ** Signature

mkSignature ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms

mkPatternSynonyms ''TermSig

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern

-- ** Scope-safe patterns

mkFoilPattern ''Raw.VarIdent ''Raw.Pattern
deriveCoSinkable ''Raw.VarIdent ''Raw.Pattern
mkToFoilPattern ''Raw.VarIdent ''Raw.Pattern
mkFromFoilPattern ''Raw.VarIdent ''Raw.Pattern

deriveUnifiablePattern ''Raw.VarIdent ''Raw.Pattern

deriving instance GHC.Generic (TermSig scope term)

deriveGenericK ''TermSig

-- | Match 'Raw.Ident' via 'Eq'.
instance ZipMatchK Raw.VarIdent where zipMatchWithK = zipMatchViaEq

instance ZipMatchK Raw.MetaVarIdent where zipMatchWithK = zipMatchViaEq

instance ZipMatch TermSig where
  zipMatch = genericZipMatch2

-- ** Pattern synonyms

pattern App' :: AST binder (Sum TermSig q) n -> AST binder (Sum TermSig q) n -> AST binder (Sum TermSig q) n
pattern App' f x = Node (L2 (AppSig f x))

pattern Lam' :: binder n l -> AST binder (Sum TermSig q) l -> AST binder (Sum TermSig q) n
pattern Lam' binder body = Node (L2 (LamSig (ScopedAST binder body)))

pattern Let' :: AST binder (Sum TermSig q) n -> binder n l -> AST binder (Sum TermSig q) l -> AST binder (Sum TermSig q) n
pattern Let' term binder body = Node (L2 (LetSig term (ScopedAST binder body)))

pattern MetaVar' :: Raw.MetaVarIdent -> [AST binder (Sum TermSig q) n] -> AST binder (Sum TermSig q) n
pattern MetaVar' metavar args = Node (L2 (MetaVarSig metavar args))

-- pattern MetaSubst' :: MetaVarIdent -> AST binder (Sum TermSig q) n -> AST binder (Sum TermSig q) n
-- pattern MetaSubst' metavar term = Node (L2 (MetaSubstSig metavar term))

-- FV( (λ x. x) y )  =  { y }
--
-- λs. λz. s (s z)    :: Term VoidS
--     λz. s (s z)    :: Term n1      --  n1 ~ { s }
--         s (s z)    :: Term n2      --  n2 ~ { s, z }
-- λs                 :: NameBinder VoidS n1
--     λz             :: NameBinder n1 n2
--

-- * User-defined code

data MetaAppSig metavar scope term = MetaAppSig metavar [term]
  deriving (Functor, Foldable, Traversable, GHC.Generic)

deriveBifunctor ''MetaAppSig
deriveBifoldable ''MetaAppSig
deriveBitraversable ''MetaAppSig

deriveGenericK ''MetaAppSig

instance (ZipMatchK a) => ZipMatch (MetaAppSig a) where
  zipMatch = genericZipMatch2

-- >>> a = "λy.(λx.λy.X[x, y X[y, x]])y" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> b = "λz.(λx.λy.X[x, y X[y, x]])z" :: MetaTerm Raw.MetaVarIdent Foil.VoidS
-- >>> alphaEquiv Foil.emptyScope a b
-- True

pattern MetaApp :: metavar -> [AST binder (Sum p (MetaAppSig metavar)) n] -> AST binder (Sum p (MetaAppSig metavar)) n
pattern MetaApp metavar args = Node (R2 (MetaAppSig metavar args))

type AST' = AST FoilPattern

-- | Scope-safe λ-term representation in scope @n@.
type Term = AST' TermSig

type SOAS metavar sig n = AST' (Sum sig (MetaAppSig metavar)) n

type MetaTerm metavar n = SOAS metavar TermSig n

data MetaAbs sig where
  MetaAbs :: NameBinderList Foil.VoidS n -> AST' sig n -> MetaAbs sig

newtype MetaSubst sig metavar metavar' = MetaSubst {getMetaSubst :: (metavar, MetaAbs (Sum sig (MetaAppSig metavar')))}

newtype MetaSubsts sig metavar metavar' = MetaSubsts
  { getSubsts :: [MetaSubst sig metavar metavar']
  }

-- M[g, \z. z a]
-- M[x, y] -> y x
-- y = \z. z a
-- x = g
-- (\z. z a) g

-- >>> subst = "X [x0, x1] ↦ x1 x0" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- >>> term = "λg. λa. λw. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . λ x2 . x0 x1
-- >>> subst = "X [x, y] ↦ (λ z . y z) x" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- >>> term = "λg. λa. λw. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . λ x2 . x0 x1
-- >>> term = "λg. λa. X[g, λz. z a]"
-- >>> nfMetaTermWithEmptyScope $ applyMetaSubsts id Foil.emptyScope (MetaSubsts [subst]) term
-- λ x0 . λ x1 . x0 x1
applyMetaSubsts ::
  (Bifunctor sig, Eq metavar, Bifunctor (MetaAppSig metavar'), Distinct n) =>
  (metavar -> metavar') ->
  Scope n ->
  MetaSubsts sig metavar metavar' ->
  SOAS metavar sig n ->
  SOAS metavar' sig n
applyMetaSubsts rename scope substs = \case
  Var x -> Var x
  Node (R2 (MetaAppSig metavar args)) ->
    let args' = map apply args
     in case lookup metavar (getMetaSubst <$> getSubsts substs) of
          Just (MetaAbs names body) ->
            let substs' =
                  nameMapToSubsts $
                    toNameMap Foil.emptyNameMap names args'
             in substitute scope substs' body
          Nothing -> Node $ R2 $ MetaAppSig (rename metavar) args'
  Node (L2 term) -> Node $ L2 $ bimap (goScopedAST rename scope substs) apply term
  where
    apply = applyMetaSubsts rename scope substs

    toNameMap :: Foil.NameMap n a -> NameBinderList n l -> [a] -> Foil.NameMap l a
    toNameMap nameMap NameBinderListEmpty [] = nameMap
    toNameMap nameMap (NameBinderListCons binder rest) (x : xs) = toNameMap fresh rest xs
      where
        fresh = Foil.addNameBinder binder x nameMap
    toNameMap _ _ _ = error "mismatched name list and argument list"

    goScopedAST ::
      (Bifunctor sig, Eq metavar, Bifunctor (MetaAppSig metavar'), Distinct n) =>
      (metavar -> metavar') ->
      Scope n ->
      MetaSubsts sig metavar metavar' ->
      ScopedAST FoilPattern (Sum sig (MetaAppSig metavar)) n ->
      ScopedAST FoilPattern (Sum sig (MetaAppSig metavar')) n
    goScopedAST rename' scope' substs' (ScopedAST binder body) =
      case assertDistinct binder of
        Foil.Distinct ->
          ScopedAST binder (applyMetaSubsts rename' newScope substs' body)
      where
        newScope = Foil.extendScopePattern binder scope'

{-# COMPLETE Var, Lam', App', Let', MetaVar', MetaApp #-}

nfMetaTerm ::
  (Foil.Distinct n) =>
  Foil.Scope n ->
  MetaTerm metavar n ->
  MetaTerm metavar n
nfMetaTerm scope = \case
  Var x -> Var x
  Lam' binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let extendedScope = Foil.extendScopePattern binder scope
         in Lam' binder (nfMetaTerm extendedScope body)
  App' f x ->
    case nfMetaTerm scope f of
      Lam' binder body ->
        let subst = matchPattern binder x
         in nfMetaTerm scope (substitute scope subst body)
      f' -> App' f' (nfMetaTerm scope x)
  Let' value binder body ->
    let subst = matchPattern binder value
        body' = substitute scope subst body
     in nfMetaTerm scope body'
  MetaVar' metavar args -> MetaVar' metavar (map (nfMetaTerm scope) args)
  -- MetaSubst' metavar term -> MetaSubst' metavar (nfMetaTerm scope term)
  MetaApp metavar args -> MetaApp metavar (map (nfMetaTerm scope) args)

nfMetaTermWithEmptyScope :: SOAS metavar TermSig VoidS -> SOAS metavar TermSig VoidS
nfMetaTermWithEmptyScope = nfMetaTerm Foil.emptyScope

nameMapToSubsts :: Foil.NameMap i (e o) -> Foil.Substitution e i o
nameMapToSubsts nameMap =
  FoilInternal.UnsafeSubstitution $ FoilInternal.getNameMap nameMap

-- ** Conversion helpers for 'MetaSubst'

toMetaSubst :: Raw.MetaSubst -> MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
toMetaSubst (Raw.AMetaSubst metavar vars term) =
  withMetaSubstVars vars Foil.emptyScope Map.empty NameBinderListEmpty $ \scope env binderList ->
    let term' = toTerm scope env (getTermFromScopedTerm term)
     in MetaSubst (metavar, MetaAbs binderList (toMetaTerm term'))

withMetaSubstVars ::
  (Distinct n) =>
  [Raw.VarIdent] ->
  Scope n ->
  Map Raw.VarIdent (Foil.Name n) ->
  NameBinderList i n ->
  ( forall l.
    (Distinct l) =>
    Scope l ->
    Map Raw.VarIdent (Foil.Name l) ->
    NameBinderList i l ->
    r
  ) ->
  r
withMetaSubstVars [] scope env binderList cont = cont scope env binderList
withMetaSubstVars (ident : idents) scope env binderList cont =
  withFresh scope $ \binder ->
    let scope' = Foil.extendScope binder scope
        name = Foil.nameOf binder
        env' = Map.insert ident name (Foil.sink <$> env)
        binderList' = push binder binderList
     in withMetaSubstVars idents scope' env' binderList' cont
  where
    push :: Foil.NameBinder i l -> NameBinderList n i -> NameBinderList n l
    push x NameBinderListEmpty = NameBinderListCons x NameBinderListEmpty
    push x (NameBinderListCons y ys) = NameBinderListCons y (push x ys)

fromMetaSubst :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent -> Raw.MetaSubst
fromMetaSubst (MetaSubst (metavar, MetaAbs binderList term)) =
  let term' = Raw.AScopedTerm $ fromTerm $ fromMetaTerm term
      idents = toVarIdentList binderList
   in Raw.AMetaSubst metavar idents term'

toVarIdentList :: NameBinderList i n -> [Raw.VarIdent]
toVarIdentList NameBinderListEmpty = []
toVarIdentList (NameBinderListCons x xs) =
  let ident = Raw.VarIdent $ "x" ++ show (Foil.nameOf x)
   in ident : toVarIdentList xs

data UnificationConstraint where
  UnificationConstraint ::
    (Distinct n) =>
    Scope n ->
    NameBinderList Foil.VoidS n ->
    MetaTerm Raw.MetaVarIdent n ->
    MetaTerm Raw.MetaVarIdent n ->
    UnificationConstraint

toUnificationConstraint :: Raw.UnificationConstraint -> UnificationConstraint
toUnificationConstraint (Raw.AUnificationConstraint vars lhs rhs) =
  withMetaSubstVars vars Foil.emptyScope Map.empty NameBinderListEmpty $ \scope env binders ->
    let toMetaTerm' = toMetaTerm . toTerm scope env . getTermFromScopedTerm
     in UnificationConstraint scope binders (toMetaTerm' lhs) (toMetaTerm' rhs)

fromUnificationConstraint :: UnificationConstraint -> Raw.UnificationConstraint
fromUnificationConstraint (UnificationConstraint _ binders lhs rhs) =
  let fromMetaTerm' = Raw.AScopedTerm . fromTerm . fromMetaTerm
   in Raw.AUnificationConstraint (toVarIdentList binders) (fromMetaTerm' lhs) (fromMetaTerm' rhs)

-- ** Conversion helpers for 'MetaTerm'

toMetaTerm :: Term n -> MetaTerm Raw.MetaVarIdent n
toMetaTerm = \case
  MetaVar metavar args -> MetaApp metavar (map toMetaTerm args)
  Var name -> Var name
  Node node -> Node (L2 (bimap toMetaScopedTerm toMetaTerm node))
  where
    toMetaScopedTerm (ScopedAST binder body) = ScopedAST binder (toMetaTerm body)

fromMetaTerm :: MetaTerm Raw.MetaVarIdent n -> Term n
fromMetaTerm = \case
  Var name -> Var name
  Node (R2 (MetaAppSig metavar args)) -> MetaVar metavar (map fromMetaTerm args)
  Node (L2 node) -> Node (bimap fromMetaScopedTerm fromMetaTerm node)
  where
    fromMetaScopedTerm (ScopedAST binder body) = ScopedAST binder (fromMetaTerm body)

-- ** Conversion helpers

-- | Convert 'Raw.Term'' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.VarIdent (Foil.Name n) -> Raw.Term -> Term n
toTerm = convertToAST convertToTermSig toFoilPattern getTermFromScopedTerm

-- | Convert 'Raw.Term'' into a closed scope-safe term.
-- This is a special case of 'toTerm''.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

fromTerm :: Term n -> Raw.Term
fromTerm =
  convertFromAST
    convertFromTermSig
    Raw.Var
    fromFoilPattern
    Raw.AScopedTerm
    (\i -> Raw.VarIdent ("x" ++ show i))

-- >>> lam Foil.emptyScope (\x -> App (MetaVar (Raw.MetaVarIdent "X") []) (Var x))
-- λ x0 . X [] x0
lam :: (Distinct n) => Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> Term l) -> Term n
lam scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam (FoilAPattern x') (makeBody x)

lam' :: (Distinct n) => Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> MetaTerm metavar l) -> MetaTerm metavar n
lam' scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam' (FoilAPattern x') (makeBody x)

-- >>> lam Foil.emptyScope (\x -> App (Var x) (Var x))
-- λ x0 . x0 x0
instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- >>> "λy.(λx.λy.x)y" :: Term Foil.VoidS
-- λ x0 . (λ x1 . λ x2 . x1) x0
instance IsString (Term Foil.VoidS) where
  fromString :: String -> Term VoidS
  fromString = unsafeParseTerm

instance Show (MetaTerm Raw.MetaVarIdent n) where
  show :: MetaTerm Raw.MetaVarIdent n -> String
  show = Raw.printTree . fromTerm . fromMetaTerm

-- >>> a = "λy.(λx.λy.X[x, y X[y, x]])y" :: Term Foil.VoidS
-- >>> b = "λz.(λx.λy.X[x, y X[y, x]])z" :: Term Foil.VoidS
-- >>> alphaEquiv Foil.emptyScope a b
-- No instance for `UnifiablePattern FoilPattern'
--   arising from a use of `alphaEquiv'
-- In the expression: alphaEquiv emptyScope a b
-- In an equation for `it_a65v6': it_a65v6 = alphaEquiv emptyScope a b

-- λ x0 . (λ x1 . λ x2 . X [x1, x2 X [x2, x1]]) x0
instance IsString (MetaTerm Raw.MetaVarIdent Foil.VoidS) where
  fromString :: String -> MetaTerm Raw.MetaVarIdent VoidS
  fromString = toMetaTerm . unsafeParseTerm

-- >>> "X [ x, y, z ] ↦ λy.(λx.λy.X[x, y X[y, x]])y" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- X [x0, x1, x2] ↦ λ x3 . (λ x4 . λ x5 . X [x4, x5 X [x5, x4]]) x3
instance Show (MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent) where
  show :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent -> String
  show = Raw.printTree . fromMetaSubst

-- >>> "X [ x, y ] ↦ λ x . y" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- X [x0, x1] ↦ λ x2 . x1
instance IsString (MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent) where
  fromString :: String -> MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
  fromString = unsafeParseMetaSubst

unsafeParseTerm :: String -> Term Foil.VoidS
unsafeParseTerm input =
  case Raw.pTerm tokens of
    Left err -> error err
    Right term -> toTermClosed term
  where
    tokens = Raw.resolveLayout False (Raw.myLexer input)

unsafeParseMetaSubst :: String -> MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
unsafeParseMetaSubst input =
  case Raw.pMetaSubst tokens of
    Left err -> error err
    Right subst -> toMetaSubst subst
  where
    tokens = Raw.resolveLayout False (Raw.myLexer input)

-- >>> "∀ m, n. Y[m, X[n, m]] = (λ x . m (x n)) m" :: UnificationConstraint
-- ∀ x0, x1 . Y [x0, X [x1, x0]] = (λ x2 . x0 (x2 x1)) x0
instance IsString UnificationConstraint where
  fromString :: String -> UnificationConstraint
  fromString = unsafeParseUnificationConstraint

instance Show UnificationConstraint where
  show :: UnificationConstraint -> String
  show = Raw.printTree . fromUnificationConstraint

unsafeParseUnificationConstraint :: String -> UnificationConstraint
unsafeParseUnificationConstraint input =
  case Raw.pUnificationConstraint tokens of
    Left err -> error err
    Right problem -> toUnificationConstraint problem
  where
    tokens = Raw.resolveLayout False (Raw.myLexer input)

-- | Match a pattern against an term.
matchPattern :: (InjectName t) => FoilPattern n l -> t n -> Foil.Substitution t l n
matchPattern (FoilAPattern x) = Foil.addSubst Foil.identitySubst x

-- >>> whnf Foil.emptyScope (lam Foil.emptyScope (\x -> App (Var x) (Var x)))
-- λ x0 . x0 x0
-- >>> whnf Foil.emptyScope "(λs.λz.s (s z)) (λs.λz.s (s z))"
-- λ x1 . (λ x0 . λ x1 . x0 (x0 x1)) ((λ x0 . λ x1 . x0 (x0 x1)) x1)
-- >>> whnf Foil.emptyScope "(λs.λz.s (?s z)) (λs.λz.s (s z))"
-- λ x1 . (λ x0 . λ x1 . x0 (x0 x1)) (?s x1)
whnf :: (Foil.Distinct n) => Foil.Scope n -> Term n -> Term n
whnf scope = \case
  App f x ->
    case whnf scope f of
      Lam binder body ->
        let subst = matchPattern binder x
         in whnf scope (substitute scope subst body)
      f' -> App f' x
  t -> t

-- >>> nf Foil.emptyScope "λy.λz. (λx.λy.y) y z"
-- λ x0 . λ x1 . x1
-- >>> nf Foil.emptyScope "λz.λw.(λx.λy.y) z (λz.z) w"
-- λ x0 . λ x1 . x1
-- >>> nf Foil.emptyScope "(λb.λx.λy.b y x) (λx.λy.x)"
-- λ x1 . λ x2 . x2
-- >>> nf Foil.emptyScope "(λs.λz.s(s(s z)))(λb.λx.λy.b y x)(λx.λy.y)"
-- λ x2 . λ x3 . x2
-- >>> nf Foil.emptyScope "(λs.λz.s (s z)) (λs.λz.s (s z)) (λb.λy.λx.b x y) (λy.λx.x)"
-- λ x1 . λ x3 . x3
-- >>> nf Foil.emptyScope "(λ a . λ x . x) (λ a . a)"
-- λ x1 . x1
-- >>> nf Foil.emptyScope "let f = λ a . λ x . x in f (λ a . a) (λ a . a)"
-- λ x1 . x1
nf :: (Foil.Distinct n) => Foil.Scope n -> Term n -> Term n
nf scope = \case
  App f x ->
    case nf scope f of
      Lam binder body ->
        let subst = matchPattern binder x
         in nf scope (substitute scope subst body)
      f' -> App f' x
  Lam binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let extendedScope = Foil.extendScopePattern binder scope
         in Lam binder (nf extendedScope body)
  Let value binder body
    | Foil.Distinct <- Foil.assertDistinct binder ->
        let subst = matchPattern binder value
         in nf scope (substitute scope subst body)
  t -> t

interpretCommand :: Raw.Command -> IO ()
interpretCommand (Raw.CommandCompute term) =
  print $ nf Foil.emptyScope $ toTermClosed term

interpretProgram :: Raw.Program -> IO ()
interpretProgram (Raw.AProgram commands) = mapM_ interpretCommand commands

-- main :: IO ()
-- main = do
--   input <- getContents
--   let tokens = Raw.resolveLayout True $ Raw.myLexer input
--   case Raw.pProgram tokens of
--     Left err -> do
--       putStrLn "\nParse              Failed...\n"
--       putStrLn err
--       exitFailure
--     Right program -> do
--       putStrLn "\nParse Successful! Interpreting..."
--       interpretProgram program

main :: IO ()
main = do
  input <- getContents
  let tokens = Raw.resolveLayout True $ Raw.myLexer input
  case Raw.pProgram tokens of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      -- putStrLn "Tokens:"
      -- mapM_ (putStrLn . showPosToken . mkPosToken) tokens
      putStrLn err
      exitFailure
    Right program -> do
      putStrLn "\nParse Successful!"
      showTree program
  where
    showTree :: (Show a, Raw.Print a) => a -> IO ()
    showTree tree = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ Raw.printTree tree

-- ** Test framework implementation

-- >>> constraint = "∀ g, a, w. X[g, λz. z a] = g a" :: UnificationConstraint
-- >>> subst = "X [x, y] ↦ (λ z . y z) x" :: MetaSubst TermSig Raw.MetaVarIdent Raw.MetaVarIdent
-- >>> isSolved (solveUnificationConstraint constraint (MetaSubsts [subst]))
-- True
solveUnificationConstraint ::
  UnificationConstraint ->
  MetaSubsts TermSig Raw.MetaVarIdent Raw.MetaVarIdent ->
  UnificationConstraint
solveUnificationConstraint (UnificationConstraint scope binders lhs rhs) substs =
  let solve = nfMetaTerm scope . applyMetaSubsts id scope substs
   in UnificationConstraint scope binders (solve lhs) (solve rhs)

isSolved :: UnificationConstraint -> Bool
isSolved (UnificationConstraint scope _ lhs rhs) = alphaEquiv scope lhs rhs
