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
  , pListTerm
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
	| HappyAbsSyn12 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn13 (Language.Lambda.Syntax.Abs.MetaVarIdent)
	| HappyAbsSyn14 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn15 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn16 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn17 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn20 ([Language.Lambda.Syntax.Abs.Term])
	| HappyAbsSyn21 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn22 (Language.Lambda.Syntax.Abs.Pattern)

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
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,128) ([0,16384,0,0,8,0,256,0,32800,7,1024,192,32768,6144,0,49168,3,512,120,0,1024,0,32768,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,64,12,0,0,0,0,0,32800,7,0,64,0,2048,0,0,0,2048,0,0,0,0,0,0,256,48,0,0,0,64,0,0,0,4096,960,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,32768,7680,0,128,0,16384,0,32768,0,0,57352,1,32768,0,0,0,0,61444,0,128,30,0,0,0,0,0,0,0,0,16,0,0,0,32800,7,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","'('","')'","','","'.'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 37
        bit_end = (st Prelude.+ 1) Prelude.* 37
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..36]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (31) = happyShift action_30
action_0 (14) = happyGoto action_32
action_0 (15) = happyGoto action_28
action_0 (16) = happyGoto action_33
action_0 _ = happyReduce_13

action_1 (31) = happyShift action_30
action_1 (15) = happyGoto action_31
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (31) = happyShift action_30
action_2 (15) = happyGoto action_28
action_2 (16) = happyGoto action_29
action_2 _ = happyReduce_13

action_3 (23) = happyShift action_19
action_3 (33) = happyShift action_20
action_3 (34) = happyShift action_21
action_3 (35) = happyShift action_10
action_3 (36) = happyShift action_22
action_3 (12) = happyGoto action_13
action_3 (13) = happyGoto action_14
action_3 (17) = happyGoto action_27
action_3 (18) = happyGoto action_16
action_3 (19) = happyGoto action_17
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (23) = happyShift action_19
action_4 (35) = happyShift action_10
action_4 (36) = happyShift action_22
action_4 (12) = happyGoto action_13
action_4 (13) = happyGoto action_14
action_4 (18) = happyGoto action_26
action_4 (19) = happyGoto action_17
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (23) = happyShift action_19
action_5 (35) = happyShift action_10
action_5 (36) = happyShift action_22
action_5 (12) = happyGoto action_13
action_5 (13) = happyGoto action_14
action_5 (19) = happyGoto action_25
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (23) = happyShift action_19
action_6 (33) = happyShift action_20
action_6 (34) = happyShift action_21
action_6 (35) = happyShift action_10
action_6 (36) = happyShift action_22
action_6 (12) = happyGoto action_13
action_6 (13) = happyGoto action_14
action_6 (17) = happyGoto action_23
action_6 (18) = happyGoto action_16
action_6 (19) = happyGoto action_17
action_6 (20) = happyGoto action_24
action_6 _ = happyReduce_23

action_7 (23) = happyShift action_19
action_7 (33) = happyShift action_20
action_7 (34) = happyShift action_21
action_7 (35) = happyShift action_10
action_7 (36) = happyShift action_22
action_7 (12) = happyGoto action_13
action_7 (13) = happyGoto action_14
action_7 (17) = happyGoto action_15
action_7 (18) = happyGoto action_16
action_7 (19) = happyGoto action_17
action_7 (21) = happyGoto action_18
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (35) = happyShift action_10
action_8 (12) = happyGoto action_11
action_8 (22) = happyGoto action_12
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (35) = happyShift action_10
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_9

action_11 _ = happyReduce_27

action_12 (37) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_20

action_14 (29) = happyShift action_41
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_26

action_16 (23) = happyShift action_19
action_16 (35) = happyShift action_10
action_16 (36) = happyShift action_22
action_16 (12) = happyGoto action_13
action_16 (13) = happyGoto action_14
action_16 (19) = happyGoto action_36
action_16 _ = happyReduce_17

action_17 _ = happyReduce_19

action_18 (37) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (23) = happyShift action_19
action_19 (33) = happyShift action_20
action_19 (34) = happyShift action_21
action_19 (35) = happyShift action_10
action_19 (36) = happyShift action_22
action_19 (12) = happyGoto action_13
action_19 (13) = happyGoto action_14
action_19 (17) = happyGoto action_40
action_19 (18) = happyGoto action_16
action_19 (19) = happyGoto action_17
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (35) = happyShift action_10
action_20 (12) = happyGoto action_11
action_20 (22) = happyGoto action_39
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (35) = happyShift action_10
action_21 (12) = happyGoto action_11
action_21 (22) = happyGoto action_38
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_10

action_23 (25) = happyShift action_37
action_23 _ = happyReduce_24

action_24 (37) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (37) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (23) = happyShift action_19
action_26 (35) = happyShift action_10
action_26 (36) = happyShift action_22
action_26 (37) = happyAccept
action_26 (12) = happyGoto action_13
action_26 (13) = happyGoto action_14
action_26 (19) = happyGoto action_36
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (37) = happyAccept
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (27) = happyShift action_35
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (37) = happyAccept
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (23) = happyShift action_19
action_30 (33) = happyShift action_20
action_30 (34) = happyShift action_21
action_30 (35) = happyShift action_10
action_30 (36) = happyShift action_22
action_30 (12) = happyGoto action_13
action_30 (13) = happyGoto action_14
action_30 (17) = happyGoto action_34
action_30 (18) = happyGoto action_16
action_30 (19) = happyGoto action_17
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (37) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (37) = happyAccept
action_32 _ = happyFail (happyExpListPerState 32)

action_33 _ = happyReduce_11

action_34 _ = happyReduce_12

action_35 (31) = happyShift action_30
action_35 (15) = happyGoto action_28
action_35 (16) = happyGoto action_47
action_35 _ = happyReduce_13

action_36 _ = happyReduce_18

action_37 (23) = happyShift action_19
action_37 (33) = happyShift action_20
action_37 (34) = happyShift action_21
action_37 (35) = happyShift action_10
action_37 (36) = happyShift action_22
action_37 (12) = happyGoto action_13
action_37 (13) = happyGoto action_14
action_37 (17) = happyGoto action_23
action_37 (18) = happyGoto action_16
action_37 (19) = happyGoto action_17
action_37 (20) = happyGoto action_46
action_37 _ = happyReduce_23

action_38 (26) = happyShift action_45
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (28) = happyShift action_44
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (24) = happyShift action_43
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (23) = happyShift action_19
action_41 (33) = happyShift action_20
action_41 (34) = happyShift action_21
action_41 (35) = happyShift action_10
action_41 (36) = happyShift action_22
action_41 (12) = happyGoto action_13
action_41 (13) = happyGoto action_14
action_41 (17) = happyGoto action_23
action_41 (18) = happyGoto action_16
action_41 (19) = happyGoto action_17
action_41 (20) = happyGoto action_42
action_41 _ = happyReduce_23

action_42 (30) = happyShift action_50
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_22

action_44 (23) = happyShift action_19
action_44 (33) = happyShift action_20
action_44 (34) = happyShift action_21
action_44 (35) = happyShift action_10
action_44 (36) = happyShift action_22
action_44 (12) = happyGoto action_13
action_44 (13) = happyGoto action_14
action_44 (17) = happyGoto action_49
action_44 (18) = happyGoto action_16
action_44 (19) = happyGoto action_17
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (23) = happyShift action_19
action_45 (33) = happyShift action_20
action_45 (34) = happyShift action_21
action_45 (35) = happyShift action_10
action_45 (36) = happyShift action_22
action_45 (12) = happyGoto action_13
action_45 (13) = happyGoto action_14
action_45 (17) = happyGoto action_15
action_45 (18) = happyGoto action_16
action_45 (19) = happyGoto action_17
action_45 (21) = happyGoto action_48
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_25

action_47 _ = happyReduce_14

action_48 _ = happyReduce_15

action_49 (32) = happyShift action_51
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_21

action_51 (23) = happyShift action_19
action_51 (33) = happyShift action_20
action_51 (34) = happyShift action_21
action_51 (35) = happyShift action_10
action_51 (36) = happyShift action_22
action_51 (12) = happyGoto action_13
action_51 (13) = happyGoto action_14
action_51 (17) = happyGoto action_15
action_51 (18) = happyGoto action_16
action_51 (19) = happyGoto action_17
action_51 (21) = happyGoto action_52
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_16

happyReduce_9 = happySpecReduce_1  12 happyReduction_9
happyReduction_9 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn12
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  13 happyReduction_10
happyReduction_10 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn13
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn14
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  15 happyReduction_12
happyReduction_12 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  16 happyReduction_13
happyReduction_13  =  HappyAbsSyn16
		 ([]
	)

happyReduce_14 = happySpecReduce_3  16 happyReduction_14
happyReduction_14 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 17 happyReduction_15
happyReduction_15 ((HappyAbsSyn21  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 6 17 happyReduction_16
happyReduction_16 ((HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  17 happyReduction_17
happyReduction_17 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  18 happyReduction_18
happyReduction_18 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  18 happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  19 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 19 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  19 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  20 happyReduction_23
happyReduction_23  =  HappyAbsSyn20
		 ([]
	)

happyReduce_24 = happySpecReduce_1  20 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 ((:[]) happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  20 happyReduction_25
happyReduction_25 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn20
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  21 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  22 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn22
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 37 37 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 23;
	PT _ (TS _ 2) -> cont 24;
	PT _ (TS _ 3) -> cont 25;
	PT _ (TS _ 4) -> cont 26;
	PT _ (TS _ 5) -> cont 27;
	PT _ (TS _ 6) -> cont 28;
	PT _ (TS _ 7) -> cont 29;
	PT _ (TS _ 8) -> cont 30;
	PT _ (TS _ 9) -> cont 31;
	PT _ (TS _ 10) -> cont 32;
	PT _ (TS _ 11) -> cont 33;
	PT _ (TS _ 12) -> cont 34;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 35;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 36;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 37 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn14 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn15 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

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
