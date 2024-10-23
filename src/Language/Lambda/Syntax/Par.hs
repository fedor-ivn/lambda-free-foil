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
  , pUnificationConstraint
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
	| HappyAbsSyn15 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn16 (Language.Lambda.Syntax.Abs.MetaVarIdent)
	| HappyAbsSyn17 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn18 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn19 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn20 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn23 ([Language.Lambda.Syntax.Abs.Term])
	| HappyAbsSyn24 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn25 (Language.Lambda.Syntax.Abs.Pattern)
	| HappyAbsSyn26 (Language.Lambda.Syntax.Abs.MetaSubst)
	| HappyAbsSyn27 (Language.Lambda.Syntax.Abs.UnificationConstraint)
	| HappyAbsSyn28 ([Language.Lambda.Syntax.Abs.VarIdent])

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
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_33,
 happyReduce_34,
 happyReduce_35 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,180) ([0,0,16,0,0,2,0,16384,0,0,24584,6,0,49153,0,8192,6144,0,1024,816,0,128,102,0,0,4,0,0,1,0,2048,0,0,512,0,0,64,0,0,0,0,16,0,0,0,0,0,0,0,0,128,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,16384,12288,0,0,0,0,0,0,0,32800,25,0,0,1,0,8192,0,16384,0,0,0,0,0,0,0,0,8,6,0,0,0,0,2,0,0,0,0,128,102,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,12292,3,0,4,0,0,2,0,1024,0,0,64,51,0,0,2,0,8,0,0,2048,0,0,0,0,128,102,0,2048,0,0,256,0,0,0,0,2048,1632,0,256,204,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,4,0,256,0,0,52225,0,8192,6528,0,1024,816,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pMetaSubst","%start_pUnificationConstraint","%start_pListVarIdent","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","MetaSubst","UnificationConstraint","ListVarIdent","'('","')'","','","'.'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","'\8704'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 45
        bit_end = (st Prelude.+ 1) Prelude.* 45
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..44]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (37) = happyShift action_39
action_0 (17) = happyGoto action_41
action_0 (18) = happyGoto action_37
action_0 (19) = happyGoto action_42
action_0 _ = happyReduce_16

action_1 (37) = happyShift action_39
action_1 (18) = happyGoto action_40
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyShift action_39
action_2 (18) = happyGoto action_37
action_2 (19) = happyGoto action_38
action_2 _ = happyReduce_16

action_3 (29) = happyShift action_29
action_3 (39) = happyShift action_30
action_3 (40) = happyShift action_31
action_3 (43) = happyShift action_13
action_3 (44) = happyShift action_20
action_3 (15) = happyGoto action_23
action_3 (16) = happyGoto action_24
action_3 (20) = happyGoto action_36
action_3 (21) = happyGoto action_26
action_3 (22) = happyGoto action_27
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (29) = happyShift action_29
action_4 (43) = happyShift action_13
action_4 (44) = happyShift action_20
action_4 (15) = happyGoto action_23
action_4 (16) = happyGoto action_24
action_4 (21) = happyGoto action_35
action_4 (22) = happyGoto action_27
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (29) = happyShift action_29
action_5 (43) = happyShift action_13
action_5 (44) = happyShift action_20
action_5 (15) = happyGoto action_23
action_5 (16) = happyGoto action_24
action_5 (22) = happyGoto action_34
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (29) = happyShift action_29
action_6 (39) = happyShift action_30
action_6 (40) = happyShift action_31
action_6 (43) = happyShift action_13
action_6 (44) = happyShift action_20
action_6 (15) = happyGoto action_23
action_6 (16) = happyGoto action_24
action_6 (20) = happyGoto action_32
action_6 (21) = happyGoto action_26
action_6 (22) = happyGoto action_27
action_6 (23) = happyGoto action_33
action_6 _ = happyReduce_26

action_7 (29) = happyShift action_29
action_7 (39) = happyShift action_30
action_7 (40) = happyShift action_31
action_7 (43) = happyShift action_13
action_7 (44) = happyShift action_20
action_7 (15) = happyGoto action_23
action_7 (16) = happyGoto action_24
action_7 (20) = happyGoto action_25
action_7 (21) = happyGoto action_26
action_7 (22) = happyGoto action_27
action_7 (24) = happyGoto action_28
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (43) = happyShift action_13
action_8 (15) = happyGoto action_21
action_8 (25) = happyGoto action_22
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (44) = happyShift action_20
action_9 (16) = happyGoto action_18
action_9 (26) = happyGoto action_19
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (42) = happyShift action_17
action_10 (27) = happyGoto action_16
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (43) = happyShift action_13
action_11 (15) = happyGoto action_14
action_11 (28) = happyGoto action_15
action_11 _ = happyReduce_33

action_12 (43) = happyShift action_13
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_12

action_14 (31) = happyShift action_53
action_14 _ = happyReduce_34

action_15 (45) = happyAccept
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (45) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (43) = happyShift action_13
action_17 (15) = happyGoto action_14
action_17 (28) = happyGoto action_52
action_17 _ = happyReduce_33

action_18 (35) = happyShift action_51
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (45) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_13

action_21 _ = happyReduce_30

action_22 (45) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_23

action_24 (35) = happyShift action_50
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_29

action_26 (29) = happyShift action_29
action_26 (43) = happyShift action_13
action_26 (44) = happyShift action_20
action_26 (15) = happyGoto action_23
action_26 (16) = happyGoto action_24
action_26 (22) = happyGoto action_45
action_26 _ = happyReduce_20

action_27 _ = happyReduce_22

action_28 (45) = happyAccept
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (29) = happyShift action_29
action_29 (39) = happyShift action_30
action_29 (40) = happyShift action_31
action_29 (43) = happyShift action_13
action_29 (44) = happyShift action_20
action_29 (15) = happyGoto action_23
action_29 (16) = happyGoto action_24
action_29 (20) = happyGoto action_49
action_29 (21) = happyGoto action_26
action_29 (22) = happyGoto action_27
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (43) = happyShift action_13
action_30 (15) = happyGoto action_21
action_30 (25) = happyGoto action_48
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (43) = happyShift action_13
action_31 (15) = happyGoto action_21
action_31 (25) = happyGoto action_47
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (31) = happyShift action_46
action_32 _ = happyReduce_27

action_33 (45) = happyAccept
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (45) = happyAccept
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (29) = happyShift action_29
action_35 (43) = happyShift action_13
action_35 (44) = happyShift action_20
action_35 (45) = happyAccept
action_35 (15) = happyGoto action_23
action_35 (16) = happyGoto action_24
action_35 (22) = happyGoto action_45
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (45) = happyAccept
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (33) = happyShift action_44
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (45) = happyAccept
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (29) = happyShift action_29
action_39 (39) = happyShift action_30
action_39 (40) = happyShift action_31
action_39 (43) = happyShift action_13
action_39 (44) = happyShift action_20
action_39 (15) = happyGoto action_23
action_39 (16) = happyGoto action_24
action_39 (20) = happyGoto action_43
action_39 (21) = happyGoto action_26
action_39 (22) = happyGoto action_27
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (45) = happyAccept
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (45) = happyAccept
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_14

action_43 _ = happyReduce_15

action_44 (37) = happyShift action_39
action_44 (18) = happyGoto action_37
action_44 (19) = happyGoto action_62
action_44 _ = happyReduce_16

action_45 _ = happyReduce_21

action_46 (29) = happyShift action_29
action_46 (39) = happyShift action_30
action_46 (40) = happyShift action_31
action_46 (43) = happyShift action_13
action_46 (44) = happyShift action_20
action_46 (15) = happyGoto action_23
action_46 (16) = happyGoto action_24
action_46 (20) = happyGoto action_32
action_46 (21) = happyGoto action_26
action_46 (22) = happyGoto action_27
action_46 (23) = happyGoto action_61
action_46 _ = happyReduce_26

action_47 (32) = happyShift action_60
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (34) = happyShift action_59
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (30) = happyShift action_58
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (29) = happyShift action_29
action_50 (39) = happyShift action_30
action_50 (40) = happyShift action_31
action_50 (43) = happyShift action_13
action_50 (44) = happyShift action_20
action_50 (15) = happyGoto action_23
action_50 (16) = happyGoto action_24
action_50 (20) = happyGoto action_32
action_50 (21) = happyGoto action_26
action_50 (22) = happyGoto action_27
action_50 (23) = happyGoto action_57
action_50 _ = happyReduce_26

action_51 (43) = happyShift action_13
action_51 (15) = happyGoto action_14
action_51 (28) = happyGoto action_56
action_51 _ = happyReduce_33

action_52 (32) = happyShift action_55
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (43) = happyShift action_13
action_53 (15) = happyGoto action_14
action_53 (28) = happyGoto action_54
action_53 _ = happyReduce_33

action_54 _ = happyReduce_35

action_55 (29) = happyShift action_29
action_55 (39) = happyShift action_30
action_55 (40) = happyShift action_31
action_55 (43) = happyShift action_13
action_55 (44) = happyShift action_20
action_55 (15) = happyGoto action_23
action_55 (16) = happyGoto action_24
action_55 (20) = happyGoto action_25
action_55 (21) = happyGoto action_26
action_55 (22) = happyGoto action_27
action_55 (24) = happyGoto action_67
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (36) = happyShift action_66
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (36) = happyShift action_65
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_25

action_59 (29) = happyShift action_29
action_59 (39) = happyShift action_30
action_59 (40) = happyShift action_31
action_59 (43) = happyShift action_13
action_59 (44) = happyShift action_20
action_59 (15) = happyGoto action_23
action_59 (16) = happyGoto action_24
action_59 (20) = happyGoto action_64
action_59 (21) = happyGoto action_26
action_59 (22) = happyGoto action_27
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (29) = happyShift action_29
action_60 (39) = happyShift action_30
action_60 (40) = happyShift action_31
action_60 (43) = happyShift action_13
action_60 (44) = happyShift action_20
action_60 (15) = happyGoto action_23
action_60 (16) = happyGoto action_24
action_60 (20) = happyGoto action_25
action_60 (21) = happyGoto action_26
action_60 (22) = happyGoto action_27
action_60 (24) = happyGoto action_63
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_28

action_62 _ = happyReduce_17

action_63 _ = happyReduce_18

action_64 (38) = happyShift action_70
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_24

action_66 (41) = happyShift action_69
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (34) = happyShift action_68
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (29) = happyShift action_29
action_68 (39) = happyShift action_30
action_68 (40) = happyShift action_31
action_68 (43) = happyShift action_13
action_68 (44) = happyShift action_20
action_68 (15) = happyGoto action_23
action_68 (16) = happyGoto action_24
action_68 (20) = happyGoto action_25
action_68 (21) = happyGoto action_26
action_68 (22) = happyGoto action_27
action_68 (24) = happyGoto action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_29
action_69 (39) = happyShift action_30
action_69 (40) = happyShift action_31
action_69 (43) = happyShift action_13
action_69 (44) = happyShift action_20
action_69 (15) = happyGoto action_23
action_69 (16) = happyGoto action_24
action_69 (20) = happyGoto action_25
action_69 (21) = happyGoto action_26
action_69 (22) = happyGoto action_27
action_69 (24) = happyGoto action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (29) = happyShift action_29
action_70 (39) = happyShift action_30
action_70 (40) = happyShift action_31
action_70 (43) = happyShift action_13
action_70 (44) = happyShift action_20
action_70 (15) = happyGoto action_23
action_70 (16) = happyGoto action_24
action_70 (20) = happyGoto action_25
action_70 (21) = happyGoto action_26
action_70 (22) = happyGoto action_27
action_70 (24) = happyGoto action_71
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_19

action_72 _ = happyReduce_31

action_73 _ = happyReduce_32

happyReduce_12 = happySpecReduce_1  15 happyReduction_12
happyReduction_12 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn15
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn16
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  18 happyReduction_15
happyReduction_15 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  19 happyReduction_16
happyReduction_16  =  HappyAbsSyn19
		 ([]
	)

happyReduce_17 = happySpecReduce_3  19 happyReduction_17
happyReduction_17 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 20 happyReduction_18
happyReduction_18 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 20 happyReduction_19
happyReduction_19 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  20 happyReduction_20
happyReduction_20 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  21 happyReduction_21
happyReduction_21 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  21 happyReduction_22
happyReduction_22 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  22 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 22 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  22 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  23 happyReduction_26
happyReduction_26  =  HappyAbsSyn23
		 ([]
	)

happyReduce_27 = happySpecReduce_1  23 happyReduction_27
happyReduction_27 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn23
		 ((:[]) happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  23 happyReduction_28
happyReduction_28 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn23
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  24 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn24
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  25 happyReduction_30
happyReduction_30 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn25
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happyReduce 6 26 happyReduction_31
happyReduction_31 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Language.Lambda.Syntax.Abs.AMetaSubst happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 6 27 happyReduction_32
happyReduction_32 ((HappyAbsSyn24  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Language.Lambda.Syntax.Abs.AUnificationConstraint happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  28 happyReduction_33
happyReduction_33  =  HappyAbsSyn28
		 ([]
	)

happyReduce_34 = happySpecReduce_1  28 happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  28 happyReduction_35
happyReduction_35 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 45 45 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 29;
	PT _ (TS _ 2) -> cont 30;
	PT _ (TS _ 3) -> cont 31;
	PT _ (TS _ 4) -> cont 32;
	PT _ (TS _ 5) -> cont 33;
	PT _ (TS _ 6) -> cont 34;
	PT _ (TS _ 7) -> cont 35;
	PT _ (TS _ 8) -> cont 36;
	PT _ (TS _ 9) -> cont 37;
	PT _ (TS _ 10) -> cont 38;
	PT _ (TS _ 11) -> cont 39;
	PT _ (TS _ 12) -> cont 40;
	PT _ (TS _ 13) -> cont 41;
	PT _ (TS _ 14) -> cont 42;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 43;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 44;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 45 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn17 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pUnificationConstraint tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

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
