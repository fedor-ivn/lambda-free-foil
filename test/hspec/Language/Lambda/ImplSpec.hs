{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Language.Lambda.ImplSpec where

import Control.Monad (forM_)
import qualified Control.Monad.Foil.Internal as Foil
import Control.Monad.Free.Foil (AST (..))
import Data.Bifunctor.Sum (Sum)
import Data.Either (isRight)
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Test.Hspec

import Data.Maybe (fromMaybe)
import Data.SOAS (AnnBinder, AnnSig, MetaAbs (..), MetaAppSig, MetaSubsts (..), push)
import qualified Data.Map as Map
import Language.Lambda.Config (Config (..), Problem (..), Solution (..))
import Language.Lambda.Framework (
  SolutionComparison (..),
  solveByMatchingAndCompareToReferenceSolutionsWith,
 )
import qualified Language.Lambda.Framework as Framework
import qualified Language.Lambda.Impl as Impl
import Language.Lambda.RawConfig (decodeConfigFile)
import qualified Language.Lambda.Syntax.Abs as Raw

type Projection = MetaAbs (AnnBinder Raw.Type Impl.FoilPattern)
  (AnnSig Raw.Type (Sum Impl.TermSig (MetaAppSig Raw.MetavarIdent))) Raw.Type

-- Deliberately choose identifiers that require renaming both parameter lists.
projection :: [Int] -> Int -> Projection
projection ids index = go Foil.emptyScope Foil.NameBinderListEmpty ids
 where
  go :: Foil.Distinct n => Foil.Scope n -> Foil.NameBinderList Foil.VoidS n -> [Int] -> Projection
  go _ binders [] = MetaAbs binders (Var (Foil.namesOfPattern binders !! index))
  go scope binders (i : rest) =
    Foil.withRefreshed scope (Foil.UnsafeName i) $ \binder ->
      go (Foil.extendScope binder scope) (push binder binders) rest

handleErr :: (Show e) => Either e a -> IO a
handleErr = either (\err -> print err >> exitFailure) pure

foo :: SpecWith ()
foo = describe "matching tests" $ do
  Config{..} <- runIO $ decodeConfigFile "problems/matching.toml" >>= handleErr
  forM_ (zip [1 ..] configProblems) $ \(i, problem) -> do
    describe ("problem #" <> show i) $ do
      result <- runIO $ handleErr (solveByMatchingAndCompareToReferenceSolutionsWith problem)
      forM_ (zip [1 ..] result) $ \(j, (_solution, comparison)) -> do
        it ("solution #" <> show j <> " should be equivalent to a reference solution") $
          comparison `shouldBe` Equivalent

spec :: Spec
spec = do
  describe "title" $ do
    Config{..} <- runIO $ decodeConfigFile "config.toml" >>= handleErr
    forM_ (zip [1 ..] configProblems) $ \(i, Problem{..}) -> do
      describe ("problem #" <> show i) $ do
        forM_ problemSolutions $ \solution@Solution{..} -> do
          it (Text.unpack $ fromMaybe "" solutionName) $ do
            Framework.validateSolution problemConstraints solution `shouldSatisfy` isRight

  foo

  describe "matching metavariable abstractions" $ do
    let types = [Raw.Base (Raw.VarIdent "a"), Raw.Base (Raw.VarIdent "b")]
    it "renames both parameter lists and transports their types" $ do
      let solutions = Impl.matchMetaAbs types Map.empty (projection [0,3] 1) (projection [1,2] 1)
      map (null . metaSubsts) solutions `shouldBe` [True]
    it "distinguishes projections after renaming both parameter lists" $ do
      let solutions = Impl.matchMetaAbs types Map.empty (projection [0,3] 0) (projection [1,2] 1)
      null solutions `shouldBe` True

  describe "moreGeneralThan (substitution comparison)" $ do
    -- Helper function to set up the test case
    let parseSubsts metavarBinders strs = do
          substs <- traverse (Framework.parseMetaSubst metavarBinders) strs
          return (MetaSubsts substs)
        testMoreGeneralThan rawMetavarBinder rawLhsSubst rawRhsSubst expectedResult = do
          Right metavarBinders <- pure $ Framework.parseMetavarBinders rawMetavarBinder
          Right lhsSubsts <- pure $ parseSubsts metavarBinders rawLhsSubst
          Right rhsSubsts <- pure $ parseSubsts metavarBinders rawRhsSubst
          let result = Impl.moreGeneralThan metavarBinders lhsSubsts rhsSubsts
          result `shouldBe` expectedResult

    context "when comparing simple substitutions" $ do
      it "should recognize that M[x] ↦ λy:t.N[x] is more general than M[x] ↦ λy:t.λz:t.x" $ do
        testMoreGeneralThan
          ["M : [t] t -> t -> t", "N : [t] t -> t"]
          ["M[x] ↦ λy:t.N[x]"]
          ["M[x] ↦ λy:t.λz:t.x"]
          True

      it "should recognize that M[x] ↦ λy:t.λz:t.x is not more general than M[x] ↦ λy:t.N[x]" $ do
        testMoreGeneralThan
          ["M : [t] t -> t -> t", "N : [t] t -> t"]
          ["M[x] ↦ λy:t.λz:t.x"]
          ["M[x] ↦ λy:t.N[x]"]
          False

    context "with identity and empty substitutions" $ do
      it "any substitution should be more general than itself" $ do
        testMoreGeneralThan
          ["M : [t] t"]
          ["M[x] ↦ x"]
          ["M[x] ↦ x"]
          True

      it "a non-empty substitution should not be more general than an empty one" $ do
        Right metavarBinders <- pure $ Framework.parseMetavarBinders ["M : [] t -> t"]
        Right lhsSubsts <- pure $ parseSubsts metavarBinders ["M[] ↦ λy:t.y"]
        let rhsSubsts = MetaSubsts []
        Impl.moreGeneralThan metavarBinders lhsSubsts rhsSubsts `shouldBe` False

    context "with multiple metavariables" $ do
      it "should correctly compare substitutions with multiple metavariables" $ do
        testMoreGeneralThan
          ["M : [t] t", "F : [t -> t, t] t"]
          ["M[x] ↦ x", "F[f, y] ↦ f y"]
          ["M[x] ↦ x"]
          False
