{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar.Par
  ( happyError
  , myLexer
  , pProgram
  , pExp3
  , pExp2
  , pExp1
  , pExp
  ) where

import Prelude

import qualified Grammar.Abs as Abs
--import qualified Grammar.Lex as Lex
import Grammar.Lex ( pattern TS, prToken, tokenPos, tokens, Tok(TI, TV), Token(..) )
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn8 (Abs.Ident)
	| HappyAbsSyn9 (Integer)
	| HappyAbsSyn10 (Abs.Program)
	| HappyAbsSyn11 (Abs.Exp)

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
 action_30 :: () => Prelude.Int -> ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16 :: () => ({-HappyReduction (Err) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,76) ([0,16,24640,16384,96,24640,16384,104,8192,0,0,0,0,0,0,16384,96,256,0,0,26688,0,32,0,0,1,24640,0,0,0,0,4,26688,0,0,24640,0,2,128,0,0,26688,16384,96,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pExp3","%start_pExp2","%start_pExp1","%start_pExp","Ident","Integer","Program","Exp3","Exp2","Exp1","Exp","'('","')'","'+'","'->'","'='","'\\\\'","'main'","L_Ident","L_integ","%eof"]
        bit_start = st Prelude.* 24
        bit_end = (st Prelude.+ 1) Prelude.* 24
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..23]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (21) = happyShift action_20
action_0 (10) = happyGoto action_19
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_13
action_1 (22) = happyShift action_6
action_1 (23) = happyShift action_15
action_1 (8) = happyGoto action_7
action_1 (9) = happyGoto action_8
action_1 (11) = happyGoto action_18
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_13
action_2 (22) = happyShift action_6
action_2 (23) = happyShift action_15
action_2 (8) = happyGoto action_7
action_2 (9) = happyGoto action_8
action_2 (11) = happyGoto action_9
action_2 (12) = happyGoto action_17
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (15) = happyShift action_13
action_3 (22) = happyShift action_6
action_3 (23) = happyShift action_15
action_3 (8) = happyGoto action_7
action_3 (9) = happyGoto action_8
action_3 (11) = happyGoto action_9
action_3 (12) = happyGoto action_10
action_3 (13) = happyGoto action_16
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (15) = happyShift action_13
action_4 (20) = happyShift action_14
action_4 (22) = happyShift action_6
action_4 (23) = happyShift action_15
action_4 (8) = happyGoto action_7
action_4 (9) = happyGoto action_8
action_4 (11) = happyGoto action_9
action_4 (12) = happyGoto action_10
action_4 (13) = happyGoto action_11
action_4 (14) = happyGoto action_12
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (22) = happyShift action_6
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_5

action_7 _ = happyReduce_8

action_8 _ = happyReduce_9

action_9 _ = happyReduce_12

action_10 (15) = happyShift action_13
action_10 (22) = happyShift action_6
action_10 (23) = happyShift action_15
action_10 (8) = happyGoto action_7
action_10 (9) = happyGoto action_8
action_10 (11) = happyGoto action_22
action_10 _ = happyReduce_14

action_11 (17) = happyShift action_23
action_11 _ = happyReduce_16

action_12 (24) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_13
action_13 (20) = happyShift action_14
action_13 (22) = happyShift action_6
action_13 (23) = happyShift action_15
action_13 (8) = happyGoto action_7
action_13 (9) = happyGoto action_8
action_13 (11) = happyGoto action_9
action_13 (12) = happyGoto action_10
action_13 (13) = happyGoto action_11
action_13 (14) = happyGoto action_25
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (22) = happyShift action_6
action_14 (8) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 (17) = happyShift action_23
action_16 (24) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (15) = happyShift action_13
action_17 (22) = happyShift action_6
action_17 (23) = happyShift action_15
action_17 (24) = happyAccept
action_17 (8) = happyGoto action_7
action_17 (9) = happyGoto action_8
action_17 (11) = happyGoto action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (24) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (24) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (19) = happyShift action_21
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (15) = happyShift action_13
action_21 (20) = happyShift action_14
action_21 (22) = happyShift action_6
action_21 (23) = happyShift action_15
action_21 (8) = happyGoto action_7
action_21 (9) = happyGoto action_8
action_21 (11) = happyGoto action_9
action_21 (12) = happyGoto action_10
action_21 (13) = happyGoto action_11
action_21 (14) = happyGoto action_29
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_11

action_23 (15) = happyShift action_13
action_23 (22) = happyShift action_6
action_23 (23) = happyShift action_15
action_23 (8) = happyGoto action_7
action_23 (9) = happyGoto action_8
action_23 (11) = happyGoto action_9
action_23 (12) = happyGoto action_28
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (18) = happyShift action_27
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (16) = happyShift action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_10

action_27 (15) = happyShift action_13
action_27 (20) = happyShift action_14
action_27 (22) = happyShift action_6
action_27 (23) = happyShift action_15
action_27 (8) = happyGoto action_7
action_27 (9) = happyGoto action_8
action_27 (11) = happyGoto action_9
action_27 (12) = happyGoto action_10
action_27 (13) = happyGoto action_11
action_27 (14) = happyGoto action_30
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (15) = happyShift action_13
action_28 (22) = happyShift action_6
action_28 (23) = happyShift action_15
action_28 (8) = happyGoto action_7
action_28 (9) = happyGoto action_8
action_28 (11) = happyGoto action_22
action_28 _ = happyReduce_13

action_29 _ = happyReduce_7

action_30 _ = happyReduce_15

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn8
		 (Abs.Ident happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn9
		 ((read happy_var_1) :: Integer
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  10 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_3)
	_
	_
	 =  HappyAbsSyn10
		 (Abs.Program happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  11 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn11
		 (Abs.EId happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  11 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn11
		 (Abs.EInt happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  11 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  12 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Abs.EApp happy_var_1 happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  12 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  13 happyReduction_13
happyReduction_13 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (Abs.EAdd happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  13 happyReduction_14
happyReduction_14 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 14 happyReduction_15
happyReduction_15 ((HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (Abs.EAbs happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  14 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 15;
	PT _ (TS _ 2) -> cont 16;
	PT _ (TS _ 3) -> cont 17;
	PT _ (TS _ 4) -> cont 18;
	PT _ (TS _ 5) -> cont 19;
	PT _ (TS _ 6) -> cont 20;
	PT _ (TS _ 7) -> cont 21;
	PT _ (TV happy_dollar_dollar) -> cont 22;
	PT _ (TI happy_dollar_dollar) -> cont 23;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 24 tk tks = happyError' (tks, explist)
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
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

pExp3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

pExp2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

pExp1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

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
