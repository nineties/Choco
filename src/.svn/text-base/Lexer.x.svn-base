{
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Lexer (
    Token(..), lexer, lexToken,
    P(..), PResult(..), runP,
    failLocMsgP
    ) where
import Var
import SrcLoc
import qualified Data.Map as M
import Panic

import Debug.Trace

{- MinCaml lexer -}
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

mincaml :-

$blank+     ;
"(*"        { nested_comment }

"_" { token Tunderscore }

$lowercase $identchar*  { strtoken $ scanWord }
@int_literal    { strtoken (Tint . read) }
@float_literal  { strtoken Tfloat }

"-" { token Tminus }
"+"     { token Tplus }
"*"     { token Ttimes }
"/"     { token Tdiv }
"-."    { token Tminusdot }
"+."    { token Tplusdot }
"*."    { token Tastdot }
"/."    { token Tslashdot }
"="     { token Tequal }
"<>"    { token Tnotequal }
"<="    { token Tlessequal }
">="    { token Tgreaterequal }
"<"     { token Tless }
">"     { token Tgreater }
","     { token Tcomma }
"."     { token Tdot }
";"     { token Tsemi }
";;"    { token Tsemisemi }
"("     { token Tlparen }
")"     { token Trparen }
"<-"    { token Tlessminus }


{
------------------------------------------------------------
-- The Token type
------------------------------------------------------------

data Token
    -- reserved words
    = Ttrue | Tfalse
    | Tif | Tthen | Telse 
    | Tlet | Trec | Tin

    -- symbols
    | Tunderscore | Tminus | Tplus | Ttimes | Tdiv
    | Tminusdot | Tplusdot | Tastdot | Tslashdot
    | Tequal | Tnotequal | Tlessequal | Tgreaterequal
    | Tless | Tgreater
    | Tcomma | Tdot | Tsemi | Tsemisemi | Tlparen | Trparen | Tlessminus

    -- basic data types
    | Tident Name
    | Tint Int
    | Tfloat String

    | Teof
    deriving (Eq, Show)

reservedWords = M.fromList
    [ ("true",  Ttrue)
    , ("false", Tfalse)
    , ("let",   Tlet)
    , ("rec",   Trec)
    , ("in",    Tin)
    , ("if",    Tif)
    , ("then",  Tthen)
    , ("else",  Telse)
    ]

scanWord str =
    case M.lookup str reservedWords of
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


nested_comment :: Action
nested_comment loc inp len = do
    input <- getInput
    go 1 input
    where 
    go 0 input = do setInput input; lexToken
    go n input = do
        case alexGetChar input of
          Nothing -> err input
          Just (c,input) -> do
            case c of
              '*' -> do case alexGetChar input of
                            Nothing -> err input
                            Just (')',input) -> go (n-1) input
                            Just (c,_)       -> go n input
              '(' -> do case alexGetChar input of
                            Nothing -> err input
                            Just ('*',input') -> go (n+1) input'
                            Just (c,input)    -> go n input
              c -> go n input
    err (AI loc _ _ _) = failLocMsgP loc "unterminated `(*'"
}

