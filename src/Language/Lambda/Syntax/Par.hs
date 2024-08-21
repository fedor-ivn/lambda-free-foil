{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Language.Lambda.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pCommand
  , pListCommand
  , pTerm
  , pTerm1
  , pTerm2
  , pScopedTerm
  , pPattern
  ) where

import Prelude

import qualified Language.Lambda.Syntax.Abs
import Language.Lambda.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn11 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn12 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn13 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn14 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn15 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn18 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn19 (Language.Lambda.Syntax.Abs.Pattern)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,86) ([0,256,0,64,0,16,8192,112,2048,16,512,4,49280,1,16384,0,4096,0,0,0,0,0,0,0,0,0,0,128,1,0,0,0,0,1794,0,256,0,64,0,0,512,4,0,0,256,0,0,0,1794,0,0,0,0,0,0,0,0,4096,0,0,0,32,0,32,0,1,0,0,2048,28,512,7,0,0,0,0,512,0,1794,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pScopedTerm","%start_pPattern","VarIdent","Program","Command","ListCommand","Term","Term1","Term2","ScopedTerm","Pattern","'('","')'","'.'","';'","'='","'compute'","'in'","'let'","'\955'","L_VarIdent","%eof"]
        bit_start = st Prelude.* 30
        bit_end = (st Prelude.+ 1) Prelude.* 30
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..29]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (25) = happyShift action_25
action_0 (12) = happyGoto action_27
action_0 (13) = happyGoto action_23
action_0 (14) = happyGoto action_28
action_0 _ = happyReduce_11

action_1 (25) = happyShift action_25
action_1 (13) = happyGoto action_26
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_25
action_2 (13) = happyGoto action_23
action_2 (14) = happyGoto action_24
action_2 _ = happyReduce_11

action_3 (20) = happyShift action_17
action_3 (27) = happyShift action_18
action_3 (28) = happyShift action_19
action_3 (29) = happyShift action_9
action_3 (11) = happyGoto action_12
action_3 (15) = happyGoto action_22
action_3 (16) = happyGoto action_14
action_3 (17) = happyGoto action_15
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (20) = happyShift action_17
action_4 (29) = happyShift action_9
action_4 (11) = happyGoto action_12
action_4 (16) = happyGoto action_21
action_4 (17) = happyGoto action_15
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (20) = happyShift action_17
action_5 (29) = happyShift action_9
action_5 (11) = happyGoto action_12
action_5 (17) = happyGoto action_20
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (20) = happyShift action_17
action_6 (27) = happyShift action_18
action_6 (28) = happyShift action_19
action_6 (29) = happyShift action_9
action_6 (11) = happyGoto action_12
action_6 (15) = happyGoto action_13
action_6 (16) = happyGoto action_14
action_6 (17) = happyGoto action_15
action_6 (18) = happyGoto action_16
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (29) = happyShift action_9
action_7 (11) = happyGoto action_10
action_7 (19) = happyGoto action_11
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (29) = happyShift action_9
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_8

action_10 _ = happyReduce_21

action_11 (30) = happyAccept
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_18

action_13 _ = happyReduce_20

action_14 (20) = happyShift action_17
action_14 (29) = happyShift action_9
action_14 (11) = happyGoto action_12
action_14 (17) = happyGoto action_31
action_14 _ = happyReduce_15

action_15 _ = happyReduce_17

action_16 (30) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (20) = happyShift action_17
action_17 (27) = happyShift action_18
action_17 (28) = happyShift action_19
action_17 (29) = happyShift action_9
action_17 (11) = happyGoto action_12
action_17 (15) = happyGoto action_34
action_17 (16) = happyGoto action_14
action_17 (17) = happyGoto action_15
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (29) = happyShift action_9
action_18 (11) = happyGoto action_10
action_18 (19) = happyGoto action_33
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (29) = happyShift action_9
action_19 (11) = happyGoto action_10
action_19 (19) = happyGoto action_32
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (30) = happyAccept
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (20) = happyShift action_17
action_21 (29) = happyShift action_9
action_21 (30) = happyAccept
action_21 (11) = happyGoto action_12
action_21 (17) = happyGoto action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (30) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (23) = happyShift action_30
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (30) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (20) = happyShift action_17
action_25 (27) = happyShift action_18
action_25 (28) = happyShift action_19
action_25 (29) = happyShift action_9
action_25 (11) = happyGoto action_12
action_25 (15) = happyGoto action_29
action_25 (16) = happyGoto action_14
action_25 (17) = happyGoto action_15
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (30) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (30) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_9

action_29 _ = happyReduce_10

action_30 (25) = happyShift action_25
action_30 (13) = happyGoto action_23
action_30 (14) = happyGoto action_38
action_30 _ = happyReduce_11

action_31 _ = happyReduce_16

action_32 (22) = happyShift action_37
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (24) = happyShift action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (21) = happyShift action_35
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_19

action_36 (20) = happyShift action_17
action_36 (27) = happyShift action_18
action_36 (28) = happyShift action_19
action_36 (29) = happyShift action_9
action_36 (11) = happyGoto action_12
action_36 (15) = happyGoto action_40
action_36 (16) = happyGoto action_14
action_36 (17) = happyGoto action_15
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (20) = happyShift action_17
action_37 (27) = happyShift action_18
action_37 (28) = happyShift action_19
action_37 (29) = happyShift action_9
action_37 (11) = happyGoto action_12
action_37 (15) = happyGoto action_13
action_37 (16) = happyGoto action_14
action_37 (17) = happyGoto action_15
action_37 (18) = happyGoto action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_12

action_39 _ = happyReduce_13

action_40 (26) = happyShift action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (20) = happyShift action_17
action_41 (27) = happyShift action_18
action_41 (28) = happyShift action_19
action_41 (29) = happyShift action_9
action_41 (11) = happyGoto action_12
action_41 (15) = happyGoto action_13
action_41 (16) = happyGoto action_14
action_41 (17) = happyGoto action_15
action_41 (18) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_14

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn11
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn12
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  13 happyReduction_10
happyReduction_10 (HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_0  14 happyReduction_11
happyReduction_11  =  HappyAbsSyn14
		 ([]
	)

happyReduce_12 = happySpecReduce_3  14 happyReduction_12
happyReduction_12 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn14
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 4 15 happyReduction_13
happyReduction_13 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 6 15 happyReduction_14
happyReduction_14 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  15 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  16 happyReduction_16
happyReduction_16 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  16 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  17 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  17 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  18 happyReduction_20
happyReduction_20 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn18
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  19 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 30 30 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 20;
	PT _ (TS _ 2) -> cont 21;
	PT _ (TS _ 3) -> cont 22;
	PT _ (TS _ 4) -> cont 23;
	PT _ (TS _ 5) -> cont 24;
	PT _ (TS _ 6) -> cont 25;
	PT _ (TS _ 7) -> cont 26;
	PT _ (TS _ 8) -> cont 27;
	PT _ (TS _ 9) -> cont 28;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 29;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 30 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
