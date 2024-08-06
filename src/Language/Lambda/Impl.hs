{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Control.Monad.Foil            (Distinct)
import qualified Control.Monad.Foil            as Foil
import           Control.Monad.Free.Foil
import           Control.Monad.Free.Foil.TH
import           Data.Bifunctor.TH
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.String                   (IsString (..))
import qualified Language.Lambda.Syntax.Abs    as Raw
import qualified Language.Lambda.Syntax.Layout as Raw
import qualified Language.Lambda.Syntax.Lex    as Raw
import qualified Language.Lambda.Syntax.Par    as Raw
import qualified Language.Lambda.Syntax.Print  as Raw
import           System.Exit                   (exitFailure)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDataKinds
-- >>> import qualified Control.Monad.Foil as Foil
-- >>> import Control.Monad.Free.Foil
-- >>> import Data.String (fromString)

-- * Generated code

-- ** Signature

mkSignature ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
-- data TermSig scope term
--   = LamSig scope
--   | AppSig term term

deriveZipMatch ''TermSig
deriveBifunctor ''TermSig
deriveBifoldable ''TermSig
deriveBitraversable ''TermSig

-- ** Pattern synonyms

mkPatternSynonyms ''TermSig
-- FV( (λ x. x) y )  =  { y }
--
-- λs. λz. s (s z)    :: Term VoidS
--     λz. s (s z)    :: Term n1      --  n1 ~ { s }
--         s (s z)    :: Term n2      --  n2 ~ { s, z }
-- λs                 :: NameBinder VoidS n1
--     λz             :: NameBinder n1 n2
--
-- pattern Lam :: Foil.NameBinder n l -> AST TermSig l -> AST TermSig n
-- pattern Lam :: Foil.NameBinder n l -> Term l -> Term n
-- pattern Lam b x = Node (LamSig (ScopedAST b x))
--
-- pattern App :: AST TermSig n -> AST TermSig n -> AST TermSig n
-- pattern App :: Term n -> Term n -> Term n
-- pattern App x0 x1 = Node (AppSig x0 x1)

-- ** Conversion helpers

mkConvertToFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern
mkConvertFromFreeFoil ''Raw.Term ''Raw.VarIdent ''Raw.ScopedTerm ''Raw.Pattern

-- * User-defined code

-- | Scope-safe λ-term representation in scope @n@.
type Term n = AST TermSig n

-- ** Conversion helpers

-- | Convert 'Raw.Term'' into a scope-safe term.
-- This is a special case of 'convertToAST'.
toTerm :: (Foil.Distinct n) => Foil.Scope n -> Map Raw.VarIdent (Foil.Name n) -> Raw.Term -> Term n
toTerm = convertToAST convertToTermSig getPatternBinder getTermFromScopedTerm

-- | Convert 'Raw.Term'' into a closed scope-safe term.
-- This is a special case of 'toTerm''.
toTermClosed :: Raw.Term -> Term Foil.VoidS
toTermClosed = toTerm Foil.emptyScope Map.empty

fromTerm :: Term n -> Raw.Term
fromTerm = convertFromAST convertFromTermSig
  Raw.Var
  Raw.APattern
  Raw.AScopedTerm
  (\i -> Raw.VarIdent ("x" ++ show i))

lam :: Distinct n => Foil.Scope n -> (forall l. (Foil.DExt n l) => Foil.Name l -> Term l) -> Term n
lam scope makeBody = Foil.withFresh scope $ \x' ->
  let x = Foil.nameOf x'
   in Lam x' (makeBody x)

-- >>> lam Foil.emptyScope (\x -> App (Var x) (Var x))
-- λ x0 . x0 x0
instance Show (Term n) where
  show = Raw.printTree . fromTerm

-- >>> "λy.(λx.λy.x)y" :: Term Foil.VoidS
-- λ x0 . (λ x1 . λ x2 . x1) x0
instance IsString (Term Foil.VoidS) where
  fromString = unsafeParseTerm

unsafeParseTerm :: String -> Term Foil.VoidS
unsafeParseTerm input =
  case Raw.pTerm tokens of
    Left err   -> error err
    Right term -> toTermClosed term
  where
    tokens = Raw.resolveLayout False (Raw.myLexer input)

-- >>> whnf Foil.emptyScope (lam Foil.emptyScope (\x -> App (Var x) (Var x)))
-- λ x0 . x0 x0
-- >>> whnf Foil.emptyScope "(λs.λz.s (s z)) (λs.λz.s (s z))"
-- λ x1 . (λ x0 . λ x1 . x0 (x0 x1)) ((λ x0 . λ x1 . x0 (x0 x1)) x1)
whnf :: (Foil.Distinct n) => Foil.Scope n -> Term n -> Term n
whnf scope = \case
  App f x ->
    case whnf scope f of
      Lam binder body ->
        let subst = Foil.addSubst Foil.identitySubst binder x
         in whnf scope (substitute scope subst body)
      f' -> App f' x
  t -> t

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
