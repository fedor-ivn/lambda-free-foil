{-# LANGUAGE OverloadedStrings #-}

module Language.Lambda.FCU.Unification where

import Data.Maybe (isJust)
import Language.Lambda.FCU.RTerms (RTerm (..), toRTerm, toTerm)
import Language.Lambda.FCU.Terms (Term (..), Id, isMeta)
import Language.Lambda.FCU.Substitutions (Substitutions (..), applySubstitutions)

------- Unification ----- bvs (th (s,t)) = Q for all, (subs, S)
unify :: [Id] -> (Substitutions, (Term, Term)) -> Maybe Substitutions
unify bvs (th, (s, t)) = case (s, t) of
    (O x, O y) -> unifyIdent x y th
    (x :.: s', y :.: t') -> unifyAbstraction x y s' t' bvs th
    (f :@ x, g :@ y) 
        | not (isMeta f) && not (isMeta g) -> unifyFunction f g x y bvs th
        | isMeta f && not (isMeta g) -> unifyFlexRigid f g x y bvs th
        | not (isMeta f) && isMeta g -> unifyFlexRigid g f y x bvs th
        | isMeta f && isMeta g -> unifyFlexFlex f g x y bvs th
    _ -> Nothing


-- Helper function to unify identical vars
unifyIdent :: Id -> Id -> Substitutions -> Maybe Substitutions
unifyIdent x y th = if x == y then Just th else Nothing

-- Helper function to unify abstractions
unifyAbstraction :: Id -> Id -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyAbstraction x y s' t' bvs th = 
    if x == y 
    then unify (bvs ++ [x]) (th, (s', t')) 
    else Nothing

-- Helper function to unify functions
unifyFunction :: Term -> Term -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyFunction f g x y bvs th = 
    if f == g 
    then unify bvs (th, (x, y)) 
    else Nothing

-- Helper function to unify flexible and rigid terms
unifyFlexRigid :: Term -> Term -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyFlexRigid _ _ _ _ _ _ = error "unifyFlexRigid not implemented"


-- Helper function to unify flexible and flexible terms
unifyFlexFlex :: Term -> Term -> Term -> Term -> [Id] -> Substitutions -> Maybe Substitutions
unifyFlexFlex _ _ _ _ _ _ = error "unifyFlexFlex not implemented"


-- >>> unify [] (Substitutions [], ("x", "x"))
-- Just (Substitutions [])

-- >>> unify [] (Substitutions [], ("x", "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "z" :.: "z"))
-- Nothing

-- >>> unify [] (Substitutions [], ("x" :.: "y", "x" :.: "y"))
-- Just (Substitutions [])

-- >>> unify [] (Substitutions [], ("Fst" :@ "x", "Fst" :@ "y"))
-- Nothing

-- >>> unify [] (Substitutions [], ("Cons" :@ "x" :@ "y", "Cons" :@ "x" :@ "y"))
-- Just (Substitutions [])
