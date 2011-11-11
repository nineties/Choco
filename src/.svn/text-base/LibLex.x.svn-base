{
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LibLex  where

import Var
import SrcLoc
import Panic

import Debug.Trace
import qualified Data.Map as M
}

$blank      = [\ \t\n\r]
$lowercase  = [a-z\_]
$uppercase  = [A-Z]
$identchar  = [A-Z a-z\_\'0-9]
$symbolchar = [\!\*\+\-\.\/\<\=\>\|]

@decimal_literal    = [0-9] [0-9\_]*
@int_literal        = @decimal_literal

@float_literal      = [0-9] [0-9\_]* 
                      (\. [0-9\_]*)? 
                      ([eE] [\+\-]? [0-9] [0-9\_]*)?

libcmm :-

$blank+ ;

$lowercase $identchar* { strtoken $ scanWord }
@int_literal    { strtoken (Tint . read) }
@float_literal  { strtoken Tfloat }

"="     { token Tequal }
"-"     { token Tminus }
"+"     { token Tplus }
"*"     { token Ttimes }
"/"     { token Tdiv }
"-."    { token Tminusdot }
"+."    { token Tplusdot }
"*."    { token Tastdot }
"/."    { token Tslashdot }
"=="    { token Tequalequal }
"<>"    { token Tnotequal }
"<="    { token Tlessequal }
">="    { token Tgreaterequal }
"<"     { token Tless }
">"     { token Tgreater }
"==."   { token Tequalequaldot }
"<>."   { token Tnotequaldot }
"<=."   { token Tlessequaldot }
">=."   { token Tgreaterequaldot }
"<."    { token Tlessdot }
">."    { token Tgreaterdot }
";"     { token Tsemi }
"("     { token Tlparen }
")"     { token Trparen }

{
------------------------------------------------------------
-- The Token type
------------------------------------------------------------
data Token
    -- reserved words
    = Tfunction | Tcall | Tif | Tlet_const | Tin

    -- symbols
    | Tequal | Tminus | Tplus | Ttimes | Tdiv 
    | Tminusdot | Tplusdot | Tastdot | Tslashdot
    | Tequalequal | Tnotequal | Tlessequal | Tgreaterequal | Tless | Tgreater
    | Tequalequaldot | Tnotequaldot | Tlessequaldot | Tgreaterequaldot 
    | Tlessdot | Tgreaterdot | Tsemi | Tlparen | Trparen

    -- basic data types
    | Tident String
    | Tint Int
    | Tfloat String

    | Teof
    deriving (Eq, Show)

reservedWords = M.fromList
    [ ("function", Tfunction)
    , ("call", Tcall)
    , ("if", Tif)
    , ("let_const", Tlet_const)
    , ("in", Tin)
    ]

scanWord str = case M.lookup str reservedWords of
    Just tok    -> tok
    Nothing     -> Tident str

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
  trace ("token: " ++ show tok_) $ do
  cont tok

------------------------------------------------------------
-- Lexer actions
------------------------------------------------------------

type Action = SrcLoc -> String -> Int -> P (Located Token)

token :: Token -> Action
token t loc _ _ = return $ L loc t

strtoken :: (String -> Token) -> Action
strtoken f loc inp len = return $ L loc (f (take len inp))
}
