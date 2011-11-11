{-# OPTIONS -cpp #-}
{-# LINE 1 "LibLex.x" #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LibLexer  where

import Var
import SrcLoc
import Panic


#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#else
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
alex_base :: Array Int Int
alex_base = listArray (0,36) [-8,-6,11,95,126,203,225,236,258,-40,290,213,312,-28,-27,-26,-25,0,0,0,0,-24,-55,-23,10,-21,-19,12,0,0,0,0,0,0,0,0,0]

alex_table :: Array Int Int
alex_table = listArray (0,567) [0,4,4,4,4,4,21,4,10,10,10,10,10,10,10,10,10,10,17,18,19,20,28,29,4,30,4,31,0,0,0,0,35,36,15,14,0,13,0,16,5,5,5,5,5,5,5,5,5,5,3,34,24,22,27,10,32,0,33,3,3,3,3,3,3,3,3,3,3,0,0,25,23,26,0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,0,0,4,0,0,0,3,3,3,3,3,3,3,3,3,3,0,0,0,0,0,4,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,7,0,6,6,6,6,6,6,6,6,6,6,9,9,9,9,9,9,9,9,9,9,7,12,6,6,6,6,6,6,6,6,6,6,0,8,8,8,8,8,8,8,8,8,8,12,0,0,0,6,0,0,0,0,0,12,12,8,8,8,8,8,8,8,8,8,8,0,0,0,0,6,0,0,0,0,0,12,12,0,0,0,8,0,0,0,0,0,12,10,10,10,10,10,10,10,10,10,10,0,0,0,0,0,8,0,11,0,11,0,12,9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

alex_check :: Array Int Int
alex_check = listArray (0,567) [-1,9,10,9,10,13,61,13,48,49,50,51,52,53,54,55,56,57,46,46,46,46,46,46,32,46,32,46,-1,-1,-1,-1,40,41,42,43,-1,45,-1,47,48,49,50,51,52,53,54,55,56,57,39,59,60,61,62,95,46,-1,46,48,49,50,51,52,53,54,55,56,57,-1,-1,61,62,61,-1,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,39,9,10,-1,-1,13,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,32,-1,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,-1,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,-1,-1,-1,95,-1,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,46,-1,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,46,69,48,49,50,51,52,53,54,55,56,57,-1,48,49,50,51,52,53,54,55,56,57,69,-1,-1,-1,95,-1,-1,-1,-1,-1,101,69,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,95,-1,-1,-1,-1,-1,101,69,-1,-1,-1,95,-1,-1,-1,-1,-1,101,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,95,-1,43,-1,45,-1,101,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,95,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_deflt :: Array Int Int
alex_deflt = listArray (0,36) [-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]

alex_accept = listArray (0::Int,36) [[],[],[(AlexAcc (alex_action_0))],[(AlexAcc (alex_action_0))],[],[(AlexAcc (alex_action_1))],[(AlexAcc (alex_action_1))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_2))],[],[],[(AlexAcc (alex_action_3))],[(AlexAcc (alex_action_4))],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))],[(AlexAcc (alex_action_7))],[(AlexAcc (alex_action_8))],[(AlexAcc (alex_action_9))],[(AlexAcc (alex_action_10))],[(AlexAcc (alex_action_11))],[],[(AlexAcc (alex_action_12))],[(AlexAcc (alex_action_15))],[(AlexAcc (alex_action_13))],[(AlexAcc (alex_action_14))],[(AlexAcc (alex_action_16))],[(AlexAcc (alex_action_17))],[(AlexAcc (alex_action_18))],[(AlexAcc (alex_action_19))],[(AlexAcc (alex_action_20))],[(AlexAcc (alex_action_21))],[(AlexAcc (alex_action_22))],[(AlexAcc (alex_action_23))],[(AlexAcc (alex_action_24))],[(AlexAcc (alex_action_25))]]
{-# LINE 59 "LibLex.x" #-}
------------------------------------------------------------
-- The Token type
------------------------------------------------------------
data Token
    -- reserved words
    = Tfunction | Tcall | Tif | Tlet_const

    -- symbols
    | Tminus | Tplus | Ttimes | Tdiv 
    | Tminusdot | Tplusdot | Tastdot | Tslashdot
    | Tequalequal | Tnotequal | Tlessequal | Tgreaterequal | Tless | Tgreater
    | Tequalequaldot | Tnotequaldot | Tlessequaldot | Tgreaterequaldot 
    | Tlessdot | Tgreaterdot | Tsemi | Tlparen | Trparen

    -- basic data types
    | Tident Name
    | Tint Int
    | Tfloat String

    | Teof
    deriving (Eq, Show)

reservedWords = M.fromlist
    [ ("function", Tfunction)
    , ("call", Tcall)
    , ("if", Tif)
    , ("let_const", Tlet_const)
    ]

scanword str = case m.lookup str reservedWords of
    Just tok    -> tok
    Nothing     -> Tident . mkName $ str

------------------------------------------------------------
-- The Parse monad
------------------------------------------------------------

data PState = PState {
    current_pos  :: !SrcLoc, -- position at current input location
    input_string :: String,  -- the current input
    prev_char    :: !Char,   -- the character before the input
    startcode    :: !Int     -- the current startcode
    }

data PResult a = POk PState a
               | PFailed SrcLoc String -- error message

data P a = P { unP :: PState -> PResult a }

runP :: String -> P a -> PResult a
runP input p = (unP p) (PState{
                current_pos = startSrcLoc,
                input_string = input,
                prev_char = '\n',
                startcode = 0}) 
        
instance Monad P where
    return a = P $ \s -> POk s a
    (P m) >>= k = P $ \s -> 
        case m s of
            POk s' a        -> (unP (k a)) s'
            PFailed pos err -> PFailed pos err
    fail = failP

failP :: String -> P a
failP msg = P $ \s -> PFailed (current_pos s) msg

failLocMsgP :: SrcLoc -> String -> P a
failLocMsgP loc msg = P $ \s -> PFailed loc msg

data AlexInput = AI !SrcLoc 
                    String  -- input
                    !Char   -- prevous char
                    Int     -- start code

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AI _ [] _ _) = Nothing
alexGetChar (AI loc (c:cs) _ i) =
    let loc' = advanceSrcLoc loc c
     in Just (c, AI loc' cs c i)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AI _ _ c _) = c

getInput :: P AlexInput
getInput = P $ \s@(PState loc inp prev code) -> POk s (AI loc inp prev code)

setInput :: AlexInput -> P ()
setInput (AI loc inp prev code) = P $ \s ->
    POk s{ current_pos = loc, 
           input_string = inp, 
           prev_char = prev,
           startcode = code } ()

lexToken :: P (Located Token)
lexToken = do
    inp@(AI loc input _ code) <- getInput
    case alexScan inp code of
        AlexEOF -> return $ L loc Teof
        AlexError (AI loc' _ _ _) -> failLocMsgP loc' "lexical error"
        AlexSkip inp2 _ -> do setInput inp2; lexToken
        AlexToken inp2 len action -> do
            setInput inp2
            action loc input len

lexer :: (Located Token -> P a) -> P a
lexer cont = do
  tok@(L _ tok_) <- lexToken
  -- trace ("token: " ++ show tok_) $ do
  cont tok

------------------------------------------------------------
-- Lexer actions
------------------------------------------------------------

type Action = SrcLoc -> String -> Int -> P (Located Token)

token :: Token -> Action
token t loc _ _ = return $ L loc t

strtoken :: (String -> Token) -> Action
strtoken f loc inp len = return $ L loc (f (take len inp))

alex_action_0 = strtoken $ scanWord 
alex_action_1 = strtoken (Tint . read) 
alex_action_2 = strtoken Tfloat 
alex_action_3 = token Tminus 
alex_action_4 = token Tplus 
alex_action_5 = token Ttimes 
alex_action_6 = token Tdiv 
alex_action_7 = token Tminusdot 
alex_action_8 = token Tplusdot 
alex_action_9 = token Tastdot 
alex_action_10 = token Tslashdot 
alex_action_11 = token Tequalequal 
alex_action_12 = token Tnotequal 
alex_action_13 = token Tlessequal 
alex_action_14 = token Tgreaterequal 
alex_action_15 = token Tless 
alex_action_16 = token Tgreater 
alex_action_17 = token Tequalequaldot 
alex_action_18 = token Tnotequaldot 
alex_action_19 = token Tlessequaldot 
alex_action_20 = token Tgreaterequaldot 
alex_action_21 = token Tlessdot 
alex_action_22 = token Tgreaterdot 
alex_action_23 = token Tsemi 
alex_action_24 = token Tlparen 
alex_action_25 = token Trparen 
{-# LINE 1 "GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 35 "GenericTemplate.hs" #-}

{-# LINE 45 "GenericTemplate.hs" #-}

{-# LINE 66 "GenericTemplate.hs" #-}
alexIndexInt16OffAddr arr off = arr ! off


{-# LINE 87 "GenericTemplate.hs" #-}
alexIndexInt32OffAddr arr off = arr ! off


{-# LINE 98 "GenericTemplate.hs" #-}
quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (sc)
  = alexScanUser undefined input (sc)

alexScanUser user input (sc)
  = case alex_scan_tkn user input (0) input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (s))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(ord_c) = ord c
		offset = (base + ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >= (0)) && (check == ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    (-1) -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len + (1)) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (len)
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastAcc a input (len)
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (len) input
	   = AlexLastSkip input (len)
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user _ _ input = 
     case alex_scan_tkn user input (0) input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (i) = i
