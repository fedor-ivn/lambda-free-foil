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
  , pType
  , pType1
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
	| HappyAbsSyn16 (Language.Lambda.Syntax.Abs.VarIdent)
	| HappyAbsSyn17 (Language.Lambda.Syntax.Abs.MetaVarIdent)
	| HappyAbsSyn18 (Language.Lambda.Syntax.Abs.Program)
	| HappyAbsSyn19 (Language.Lambda.Syntax.Abs.Command)
	| HappyAbsSyn20 ([Language.Lambda.Syntax.Abs.Command])
	| HappyAbsSyn21 (Language.Lambda.Syntax.Abs.Term)
	| HappyAbsSyn24 ([Language.Lambda.Syntax.Abs.Term])
	| HappyAbsSyn25 (Language.Lambda.Syntax.Abs.ScopedTerm)
	| HappyAbsSyn26 (Language.Lambda.Syntax.Abs.Pattern)
	| HappyAbsSyn27 (Language.Lambda.Syntax.Abs.MetaSubst)
	| HappyAbsSyn28 ([Language.Lambda.Syntax.Abs.VarIdent])
	| HappyAbsSyn29 (Language.Lambda.Syntax.Abs.Type)

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
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

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
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,178) ([0,0,256,0,0,256,0,0,256,0,16384,27648,0,16384,24576,0,16384,24576,0,16384,27648,0,16384,27648,0,0,8192,0,0,16384,0,0,8192,0,16384,8192,0,16384,8192,0,0,8192,0,0,0,0,0,0,0,0,0,0,16384,8192,0,0,0,0,0,2,0,0,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,16384,24576,0,0,0,0,0,0,0,16384,27648,0,0,8192,0,0,8192,0,0,1,0,0,0,0,0,0,0,16384,24576,0,0,0,0,0,16,0,0,0,0,16384,27648,0,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,16384,27648,0,0,8,0,0,8,0,32768,0,0,16384,27648,0,0,8192,0,0,8192,0,16384,8192,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,128,0,0,0,0,16384,8192,0,16384,8192,0,0,0,0,0,0,0,0,4,0,0,32,0,0,0,0,0,4096,0,16384,27648,0,16384,27648,0,16384,27648,0,0,0,0,0,512,0,0,0,0,16384,27648,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pCommand","%start_pListCommand","%start_pTerm","%start_pTerm1","%start_pTerm2","%start_pListTerm","%start_pScopedTerm","%start_pPattern","%start_pMetaSubst","%start_pListVarIdent","%start_pType","%start_pType1","VarIdent","MetaVarIdent","Program","Command","ListCommand","Term","Term1","Term2","ListTerm","ScopedTerm","Pattern","MetaSubst","ListVarIdent","Type","Type1","'('","')'","','","'->'","'.'","':'","';'","'='","'['","']'","'compute'","'in'","'let'","'\955'","'\8614'","L_VarIdent","L_MetaVarIdent","%eof"]
        bit_start = st Prelude.* 48
        bit_end = (st Prelude.+ 1) Prelude.* 48
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..47]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (41) = happyShift action_43
action_0 (18) = happyGoto action_45
action_0 (19) = happyGoto action_41
action_0 (20) = happyGoto action_46
action_0 _ = happyReduce_17

action_1 (41) = happyShift action_43
action_1 (19) = happyGoto action_44
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (41) = happyShift action_43
action_2 (19) = happyGoto action_41
action_2 (20) = happyGoto action_42
action_2 _ = happyReduce_17

action_3 (31) = happyShift action_33
action_3 (43) = happyShift action_34
action_3 (44) = happyShift action_35
action_3 (46) = happyShift action_14
action_3 (47) = happyShift action_24
action_3 (16) = happyGoto action_27
action_3 (17) = happyGoto action_28
action_3 (21) = happyGoto action_40
action_3 (22) = happyGoto action_30
action_3 (23) = happyGoto action_31
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (31) = happyShift action_33
action_4 (46) = happyShift action_14
action_4 (47) = happyShift action_24
action_4 (16) = happyGoto action_27
action_4 (17) = happyGoto action_28
action_4 (22) = happyGoto action_39
action_4 (23) = happyGoto action_31
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (31) = happyShift action_33
action_5 (46) = happyShift action_14
action_5 (47) = happyShift action_24
action_5 (16) = happyGoto action_27
action_5 (17) = happyGoto action_28
action_5 (23) = happyGoto action_38
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (31) = happyShift action_33
action_6 (43) = happyShift action_34
action_6 (44) = happyShift action_35
action_6 (46) = happyShift action_14
action_6 (47) = happyShift action_24
action_6 (16) = happyGoto action_27
action_6 (17) = happyGoto action_28
action_6 (21) = happyGoto action_36
action_6 (22) = happyGoto action_30
action_6 (23) = happyGoto action_31
action_6 (24) = happyGoto action_37
action_6 _ = happyReduce_27

action_7 (31) = happyShift action_33
action_7 (43) = happyShift action_34
action_7 (44) = happyShift action_35
action_7 (46) = happyShift action_14
action_7 (47) = happyShift action_24
action_7 (16) = happyGoto action_27
action_7 (17) = happyGoto action_28
action_7 (21) = happyGoto action_29
action_7 (22) = happyGoto action_30
action_7 (23) = happyGoto action_31
action_7 (25) = happyGoto action_32
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyShift action_14
action_8 (16) = happyGoto action_25
action_8 (26) = happyGoto action_26
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (47) = happyShift action_24
action_9 (17) = happyGoto action_22
action_9 (27) = happyGoto action_23
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (46) = happyShift action_14
action_10 (16) = happyGoto action_20
action_10 (28) = happyGoto action_21
action_10 _ = happyReduce_33

action_11 (31) = happyShift action_17
action_11 (46) = happyShift action_14
action_11 (16) = happyGoto action_15
action_11 (29) = happyGoto action_18
action_11 (30) = happyGoto action_19
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (31) = happyShift action_17
action_12 (46) = happyShift action_14
action_12 (16) = happyGoto action_15
action_12 (30) = happyGoto action_16
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (46) = happyShift action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_13

action_15 _ = happyReduce_38

action_16 (48) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (31) = happyShift action_17
action_17 (46) = happyShift action_14
action_17 (16) = happyGoto action_15
action_17 (29) = happyGoto action_58
action_17 (30) = happyGoto action_19
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (48) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (34) = happyShift action_57
action_19 _ = happyReduce_37

action_20 (33) = happyShift action_56
action_20 _ = happyReduce_34

action_21 (48) = happyAccept
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (39) = happyShift action_55
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (48) = happyAccept
action_23 _ = happyFail (happyExpListPerState 23)

action_24 _ = happyReduce_14

action_25 _ = happyReduce_31

action_26 (48) = happyAccept
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_24

action_28 (39) = happyShift action_54
action_28 _ = happyFail (happyExpListPerState 28)

action_29 _ = happyReduce_30

action_30 (31) = happyShift action_33
action_30 (46) = happyShift action_14
action_30 (47) = happyShift action_24
action_30 (16) = happyGoto action_27
action_30 (17) = happyGoto action_28
action_30 (23) = happyGoto action_49
action_30 _ = happyReduce_21

action_31 _ = happyReduce_23

action_32 (48) = happyAccept
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (31) = happyShift action_33
action_33 (43) = happyShift action_34
action_33 (44) = happyShift action_35
action_33 (46) = happyShift action_14
action_33 (47) = happyShift action_24
action_33 (16) = happyGoto action_27
action_33 (17) = happyGoto action_28
action_33 (21) = happyGoto action_53
action_33 (22) = happyGoto action_30
action_33 (23) = happyGoto action_31
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (46) = happyShift action_14
action_34 (16) = happyGoto action_25
action_34 (26) = happyGoto action_52
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (46) = happyShift action_14
action_35 (16) = happyGoto action_25
action_35 (26) = happyGoto action_51
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (33) = happyShift action_50
action_36 _ = happyReduce_28

action_37 (48) = happyAccept
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (48) = happyAccept
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (31) = happyShift action_33
action_39 (46) = happyShift action_14
action_39 (47) = happyShift action_24
action_39 (48) = happyAccept
action_39 (16) = happyGoto action_27
action_39 (17) = happyGoto action_28
action_39 (23) = happyGoto action_49
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (48) = happyAccept
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (37) = happyShift action_48
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (48) = happyAccept
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (31) = happyShift action_33
action_43 (43) = happyShift action_34
action_43 (44) = happyShift action_35
action_43 (46) = happyShift action_14
action_43 (47) = happyShift action_24
action_43 (16) = happyGoto action_27
action_43 (17) = happyGoto action_28
action_43 (21) = happyGoto action_47
action_43 (22) = happyGoto action_30
action_43 (23) = happyGoto action_31
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (48) = happyAccept
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (48) = happyAccept
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_15

action_47 _ = happyReduce_16

action_48 (41) = happyShift action_43
action_48 (19) = happyGoto action_41
action_48 (20) = happyGoto action_68
action_48 _ = happyReduce_17

action_49 _ = happyReduce_22

action_50 (31) = happyShift action_33
action_50 (43) = happyShift action_34
action_50 (44) = happyShift action_35
action_50 (46) = happyShift action_14
action_50 (47) = happyShift action_24
action_50 (16) = happyGoto action_27
action_50 (17) = happyGoto action_28
action_50 (21) = happyGoto action_36
action_50 (22) = happyGoto action_30
action_50 (23) = happyGoto action_31
action_50 (24) = happyGoto action_67
action_50 _ = happyReduce_27

action_51 (36) = happyShift action_66
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (36) = happyShift action_65
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (32) = happyShift action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (31) = happyShift action_33
action_54 (43) = happyShift action_34
action_54 (44) = happyShift action_35
action_54 (46) = happyShift action_14
action_54 (47) = happyShift action_24
action_54 (16) = happyGoto action_27
action_54 (17) = happyGoto action_28
action_54 (21) = happyGoto action_36
action_54 (22) = happyGoto action_30
action_54 (23) = happyGoto action_31
action_54 (24) = happyGoto action_63
action_54 _ = happyReduce_27

action_55 (46) = happyShift action_14
action_55 (16) = happyGoto action_20
action_55 (28) = happyGoto action_62
action_55 _ = happyReduce_33

action_56 (46) = happyShift action_14
action_56 (16) = happyGoto action_20
action_56 (28) = happyGoto action_61
action_56 _ = happyReduce_33

action_57 (31) = happyShift action_17
action_57 (46) = happyShift action_14
action_57 (16) = happyGoto action_15
action_57 (29) = happyGoto action_60
action_57 (30) = happyGoto action_19
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (32) = happyShift action_59
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_39

action_60 _ = happyReduce_36

action_61 _ = happyReduce_35

action_62 (40) = happyShift action_72
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (40) = happyShift action_71
action_63 _ = happyFail (happyExpListPerState 63)

action_64 _ = happyReduce_26

action_65 (31) = happyShift action_17
action_65 (46) = happyShift action_14
action_65 (16) = happyGoto action_15
action_65 (29) = happyGoto action_70
action_65 (30) = happyGoto action_19
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (31) = happyShift action_17
action_66 (46) = happyShift action_14
action_66 (16) = happyGoto action_15
action_66 (29) = happyGoto action_69
action_66 (30) = happyGoto action_19
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_29

action_68 _ = happyReduce_18

action_69 (35) = happyShift action_75
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (38) = happyShift action_74
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_25

action_72 (45) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (31) = happyShift action_33
action_73 (43) = happyShift action_34
action_73 (44) = happyShift action_35
action_73 (46) = happyShift action_14
action_73 (47) = happyShift action_24
action_73 (16) = happyGoto action_27
action_73 (17) = happyGoto action_28
action_73 (21) = happyGoto action_29
action_73 (22) = happyGoto action_30
action_73 (23) = happyGoto action_31
action_73 (25) = happyGoto action_78
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (31) = happyShift action_33
action_74 (43) = happyShift action_34
action_74 (44) = happyShift action_35
action_74 (46) = happyShift action_14
action_74 (47) = happyShift action_24
action_74 (16) = happyGoto action_27
action_74 (17) = happyGoto action_28
action_74 (21) = happyGoto action_77
action_74 (22) = happyGoto action_30
action_74 (23) = happyGoto action_31
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (31) = happyShift action_33
action_75 (43) = happyShift action_34
action_75 (44) = happyShift action_35
action_75 (46) = happyShift action_14
action_75 (47) = happyShift action_24
action_75 (16) = happyGoto action_27
action_75 (17) = happyGoto action_28
action_75 (21) = happyGoto action_29
action_75 (22) = happyGoto action_30
action_75 (23) = happyGoto action_31
action_75 (25) = happyGoto action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_19

action_77 (42) = happyShift action_79
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_32

action_79 (31) = happyShift action_33
action_79 (43) = happyShift action_34
action_79 (44) = happyShift action_35
action_79 (46) = happyShift action_14
action_79 (47) = happyShift action_24
action_79 (16) = happyGoto action_27
action_79 (17) = happyGoto action_28
action_79 (21) = happyGoto action_29
action_79 (22) = happyGoto action_30
action_79 (23) = happyGoto action_31
action_79 (25) = happyGoto action_80
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_20

happyReduce_13 = happySpecReduce_1  16 happyReduction_13
happyReduction_13 (HappyTerminal (PT _ (T_VarIdent happy_var_1)))
	 =  HappyAbsSyn16
		 (Language.Lambda.Syntax.Abs.VarIdent happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  17 happyReduction_14
happyReduction_14 (HappyTerminal (PT _ (T_MetaVarIdent happy_var_1)))
	 =  HappyAbsSyn17
		 (Language.Lambda.Syntax.Abs.MetaVarIdent happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  18 happyReduction_15
happyReduction_15 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn18
		 (Language.Lambda.Syntax.Abs.AProgram happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  19 happyReduction_16
happyReduction_16 (HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Language.Lambda.Syntax.Abs.CommandCompute happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_0  20 happyReduction_17
happyReduction_17  =  HappyAbsSyn20
		 ([]
	)

happyReduce_18 = happySpecReduce_3  20 happyReduction_18
happyReduction_18 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn20
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 6 21 happyReduction_19
happyReduction_19 ((HappyAbsSyn25  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.Lam happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 8 21 happyReduction_20
happyReduction_20 ((HappyAbsSyn25  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn26  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.Let happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_1  21 happyReduction_21
happyReduction_21 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_2  22 happyReduction_22
happyReduction_22 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.App happy_var_1 happy_var_2
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  22 happyReduction_23
happyReduction_23 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  23 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.Var happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 4 23 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Language.Lambda.Syntax.Abs.MetaVar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  23 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  24 happyReduction_27
happyReduction_27  =  HappyAbsSyn24
		 ([]
	)

happyReduce_28 = happySpecReduce_1  24 happyReduction_28
happyReduction_28 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn24
		 ((:[]) happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  24 happyReduction_29
happyReduction_29 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn24
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  25 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (Language.Lambda.Syntax.Abs.AScopedTerm happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  26 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn26
		 (Language.Lambda.Syntax.Abs.APattern happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 6 27 happyReduction_32
happyReduction_32 ((HappyAbsSyn25  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Language.Lambda.Syntax.Abs.MetaSubst happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_0  28 happyReduction_33
happyReduction_33  =  HappyAbsSyn28
		 ([]
	)

happyReduce_34 = happySpecReduce_1  28 happyReduction_34
happyReduction_34 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn28
		 ((:[]) happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  28 happyReduction_35
happyReduction_35 (HappyAbsSyn28  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn28
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  29 happyReduction_36
happyReduction_36 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (Language.Lambda.Syntax.Abs.Fun happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  29 happyReduction_37
happyReduction_37 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  30 happyReduction_38
happyReduction_38 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn29
		 (Language.Lambda.Syntax.Abs.Base happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  30 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn29
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 31;
	PT _ (TS _ 2) -> cont 32;
	PT _ (TS _ 3) -> cont 33;
	PT _ (TS _ 4) -> cont 34;
	PT _ (TS _ 5) -> cont 35;
	PT _ (TS _ 6) -> cont 36;
	PT _ (TS _ 7) -> cont 37;
	PT _ (TS _ 8) -> cont 38;
	PT _ (TS _ 9) -> cont 39;
	PT _ (TS _ 10) -> cont 40;
	PT _ (TS _ 11) -> cont 41;
	PT _ (TS _ 12) -> cont 42;
	PT _ (TS _ 13) -> cont 43;
	PT _ (TS _ 14) -> cont 44;
	PT _ (TS _ 15) -> cont 45;
	PT _ (T_VarIdent happy_dollar_dollar) -> cont 46;
	PT _ (T_MetaVarIdent happy_dollar_dollar) -> cont 47;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 48 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn18 z -> happyReturn z; _other -> notHappyAtAll })

pCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn19 z -> happyReturn z; _other -> notHappyAtAll })

pListCommand tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn20 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn21 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn24 z -> happyReturn z; _other -> notHappyAtAll })

pScopedTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

pPattern tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pMetaSubst tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pListVarIdent tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn28 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn29 z -> happyReturn z; _other -> notHappyAtAll })

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
