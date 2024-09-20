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
  , pMetaSubst
  , pListVarIdent
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
	| HappyAbsSyn14 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn15 (Language.Lambda.Syntax.Abs.MetaVarIdent)
	| HappyAbsSyn16 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn17 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn18 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn19 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn22 ([Language.Lambda.Syntax.Abs.Term])
	| HappyAbsSyn23 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn24 (Language.Lambda.Syntax.Abs.Pattern)
	| HappyAbsSyn25 (Language.Lambda.Syntax.Abs.MetaSubst)
	| HappyAbsSyn26 ([Language.Lambda.Syntax.Abs.VarIdent])

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
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,156) ([0,0,4,0,4096,0,0,64,0,256,108,0,32772,1,4096,1536,0,64,27,0,27649,0,0,128,0,0,4,0,2048,0,0,32,0,0,0,16384,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,24577,0,0,0,0,0,0,16384,6912,0,0,32,0,32768,0,16384,0,0,0,0,0,0,0,1024,384,0,0,0,0,4,0,0,0,0,45060,1,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,256,108,0,32,0,0,2,0,128,0,0,27649,0,0,128,0,0,2,0,0,0,32768,0,0,512,0,0,0,0,64,27,0,27649,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,256,0,64,27,0,27649,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pMetaSubst","%start_pListVarIdent","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","MetaSubst","ListVarIdent","'('","')'","','","'.'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 42
        bit_end = (st Prelude.+ 1) Prelude.* 42
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..41]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (35) = happyShift action_36
action_0 (16) = happyGoto action_38
action_0 (17) = happyGoto action_34
action_0 (18) = happyGoto action_39
action_0 _ = happyReduce_15

action_1 (35) = happyShift action_36
action_1 (17) = happyGoto action_37
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_36
action_2 (17) = happyGoto action_34
action_2 (18) = happyGoto action_35
action_2 _ = happyReduce_15

action_3 (27) = happyShift action_26
action_3 (37) = happyShift action_27
action_3 (38) = happyShift action_28
action_3 (40) = happyShift action_12
action_3 (41) = happyShift action_17
action_3 (14) = happyGoto action_20
action_3 (15) = happyGoto action_21
action_3 (19) = happyGoto action_33
action_3 (20) = happyGoto action_23
action_3 (21) = happyGoto action_24
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (27) = happyShift action_26
action_4 (40) = happyShift action_12
action_4 (41) = happyShift action_17
action_4 (14) = happyGoto action_20
action_4 (15) = happyGoto action_21
action_4 (20) = happyGoto action_32
action_4 (21) = happyGoto action_24
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (27) = happyShift action_26
action_5 (40) = happyShift action_12
action_5 (41) = happyShift action_17
action_5 (14) = happyGoto action_20
action_5 (15) = happyGoto action_21
action_5 (21) = happyGoto action_31
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (27) = happyShift action_26
action_6 (37) = happyShift action_27
action_6 (38) = happyShift action_28
action_6 (40) = happyShift action_12
action_6 (41) = happyShift action_17
action_6 (14) = happyGoto action_20
action_6 (15) = happyGoto action_21
action_6 (19) = happyGoto action_29
action_6 (20) = happyGoto action_23
action_6 (21) = happyGoto action_24
action_6 (22) = happyGoto action_30
action_6 _ = happyReduce_25

action_7 (27) = happyShift action_26
action_7 (37) = happyShift action_27
action_7 (38) = happyShift action_28
action_7 (40) = happyShift action_12
action_7 (41) = happyShift action_17
action_7 (14) = happyGoto action_20
action_7 (15) = happyGoto action_21
action_7 (19) = happyGoto action_22
action_7 (20) = happyGoto action_23
action_7 (21) = happyGoto action_24
action_7 (23) = happyGoto action_25
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (40) = happyShift action_12
action_8 (14) = happyGoto action_18
action_8 (24) = happyGoto action_19
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (41) = happyShift action_17
action_9 (15) = happyGoto action_15
action_9 (25) = happyGoto action_16
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (40) = happyShift action_12
action_10 (14) = happyGoto action_13
action_10 (26) = happyGoto action_14
action_10 _ = happyReduce_31

action_11 (40) = happyShift action_12
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_11

action_13 (29) = happyShift action_49
action_13 _ = happyReduce_32

action_14 (42) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (33) = happyShift action_48
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (42) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_12

action_18 _ = happyReduce_29

action_19 (42) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_22

action_21 (33) = happyShift action_47
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_28

action_23 (27) = happyShift action_26
action_23 (40) = happyShift action_12
action_23 (41) = happyShift action_17
action_23 (14) = happyGoto action_20
action_23 (15) = happyGoto action_21
action_23 (21) = happyGoto action_42
action_23 _ = happyReduce_19

action_24 _ = happyReduce_21

action_25 (42) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (27) = happyShift action_26
action_26 (37) = happyShift action_27
action_26 (38) = happyShift action_28
action_26 (40) = happyShift action_12
action_26 (41) = happyShift action_17
action_26 (14) = happyGoto action_20
action_26 (15) = happyGoto action_21
action_26 (19) = happyGoto action_46
action_26 (20) = happyGoto action_23
action_26 (21) = happyGoto action_24
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (40) = happyShift action_12
action_27 (14) = happyGoto action_18
action_27 (24) = happyGoto action_45
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (40) = happyShift action_12
action_28 (14) = happyGoto action_18
action_28 (24) = happyGoto action_44
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (29) = happyShift action_43
action_29 _ = happyReduce_26

action_30 (42) = happyAccept
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (42) = happyAccept
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (27) = happyShift action_26
action_32 (40) = happyShift action_12
action_32 (41) = happyShift action_17
action_32 (42) = happyAccept
action_32 (14) = happyGoto action_20
action_32 (15) = happyGoto action_21
action_32 (21) = happyGoto action_42
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (42) = happyAccept
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (31) = happyShift action_41
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (42) = happyAccept
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (27) = happyShift action_26
action_36 (37) = happyShift action_27
action_36 (38) = happyShift action_28
action_36 (40) = happyShift action_12
action_36 (41) = happyShift action_17
action_36 (14) = happyGoto action_20
action_36 (15) = happyGoto action_21
action_36 (19) = happyGoto action_40
action_36 (20) = happyGoto action_23
action_36 (21) = happyGoto action_24
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (42) = happyAccept
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (42) = happyAccept
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_13

action_40 _ = happyReduce_14

action_41 (35) = happyShift action_36
action_41 (17) = happyGoto action_34
action_41 (18) = happyGoto action_57
action_41 _ = happyReduce_15

action_42 _ = happyReduce_20

action_43 (27) = happyShift action_26
action_43 (37) = happyShift action_27
action_43 (38) = happyShift action_28
action_43 (40) = happyShift action_12
action_43 (41) = happyShift action_17
action_43 (14) = happyGoto action_20
action_43 (15) = happyGoto action_21
action_43 (19) = happyGoto action_29
action_43 (20) = happyGoto action_23
action_43 (21) = happyGoto action_24
action_43 (22) = happyGoto action_56
action_43 _ = happyReduce_25

action_44 (30) = happyShift action_55
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (32) = happyShift action_54
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (27) = happyShift action_26
action_47 (37) = happyShift action_27
action_47 (38) = happyShift action_28
action_47 (40) = happyShift action_12
action_47 (41) = happyShift action_17
action_47 (14) = happyGoto action_20
action_47 (15) = happyGoto action_21
action_47 (19) = happyGoto action_29
action_47 (20) = happyGoto action_23
action_47 (21) = happyGoto action_24
action_47 (22) = happyGoto action_52
action_47 _ = happyReduce_25

action_48 (40) = happyShift action_12
action_48 (14) = happyGoto action_13
action_48 (26) = happyGoto action_51
action_48 _ = happyReduce_31

action_49 (40) = happyShift action_12
action_49 (14) = happyGoto action_13
action_49 (26) = happyGoto action_50
action_49 _ = happyReduce_31

action_50 _ = happyReduce_33

action_51 (34) = happyShift action_61
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (34) = happyShift action_60
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_24

action_54 (27) = happyShift action_26
action_54 (37) = happyShift action_27
action_54 (38) = happyShift action_28
action_54 (40) = happyShift action_12
action_54 (41) = happyShift action_17
action_54 (14) = happyGoto action_20
action_54 (15) = happyGoto action_21
action_54 (19) = happyGoto action_59
action_54 (20) = happyGoto action_23
action_54 (21) = happyGoto action_24
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (27) = happyShift action_26
action_55 (37) = happyShift action_27
action_55 (38) = happyShift action_28
action_55 (40) = happyShift action_12
action_55 (41) = happyShift action_17
action_55 (14) = happyGoto action_20
action_55 (15) = happyGoto action_21
action_55 (19) = happyGoto action_22
action_55 (20) = happyGoto action_23
action_55 (21) = happyGoto action_24
action_55 (23) = happyGoto action_58
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_27

action_57 _ = happyReduce_16

action_58 _ = happyReduce_17

action_59 (36) = happyShift action_63
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_23

action_61 (39) = happyShift action_62
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (27) = happyShift action_26
action_62 (37) = happyShift action_27
action_62 (38) = happyShift action_28
action_62 (40) = happyShift action_12
action_62 (41) = happyShift action_17
action_62 (14) = happyGoto action_20
action_62 (15) = happyGoto action_21
action_62 (19) = happyGoto action_22
action_62 (20) = happyGoto action_23
action_62 (21) = happyGoto action_24
action_62 (23) = happyGoto action_65
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (27) = happyShift action_26
action_63 (37) = happyShift action_27
action_63 (38) = happyShift action_28
action_63 (40) = happyShift action_12
action_63 (41) = happyShift action_17
action_63 (14) = happyGoto action_20
action_63 (15) = happyGoto action_21
action_63 (19) = happyGoto action_22
action_63 (20) = happyGoto action_23
action_63 (21) = happyGoto action_24
action_63 (23) = happyGoto action_64
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_18

action_65 _ = happyReduce_30

happyReduce_11 = happySpecReduce_1  14 happyReduction_11
happyReduction_11 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn14
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn16
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  17 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  18 happyReduction_15
happyReduction_15  =  HappyAbsSyn18
		 ([]
	)

happyReduce_16 = happySpecReduce_3  18 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn18
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 19 happyReduction_17
happyReduction_17 ((HappyAbsSyn23  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 19 happyReduction_18
happyReduction_18 ((HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  19 happyReduction_19
happyReduction_19 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  20 happyReduction_20
happyReduction_20 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  20 happyReduction_21
happyReduction_21 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  21 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 21 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  21 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_0  22 happyReduction_25
happyReduction_25  =  HappyAbsSyn22
		 ([]
	)

happyReduce_26 = happySpecReduce_1  22 happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn22
		 ((:[]) happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  22 happyReduction_27
happyReduction_27 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn22
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  23 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn23
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  24 happyReduction_29
happyReduction_29 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn24
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happyReduce 6 25 happyReduction_30
happyReduction_30 ((HappyAbsSyn23  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Language.Lambda.Syntax.Abs.MetaSubst happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_0  26 happyReduction_31
happyReduction_31  =  HappyAbsSyn26
		 ([]
	)

happyReduce_32 = happySpecReduce_1  26 happyReduction_32
happyReduction_32 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn26
		 ((:[]) happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  26 happyReduction_33
happyReduction_33 (HappyAbsSyn26  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn26
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 42 42 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 27;
	PT _ (TS _ 2) -> cont 28;
	PT _ (TS _ 3) -> cont 29;
	PT _ (TS _ 4) -> cont 30;
	PT _ (TS _ 5) -> cont 31;
	PT _ (TS _ 6) -> cont 32;
	PT _ (TS _ 7) -> cont 33;
	PT _ (TS _ 8) -> cont 34;
	PT _ (TS _ 9) -> cont 35;
	PT _ (TS _ 10) -> cont 36;
	PT _ (TS _ 11) -> cont 37;
	PT _ (TS _ 12) -> cont 38;
	PT _ (TS _ 13) -> cont 39;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 40;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 41;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 42 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn16 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

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
