{
{-# OPTIONS_GHC -w #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module Parser (parseProgram) where

import Choco
import Common
import Const
import Lexer
import McSyn
import Outputable
import Primitive
import SrcLoc
import Var

{- MinCaml parser -}
}

%token
    '_'         { L _ Tunderscore }
    '-'         { L _ Tminus }
    '+'         { L _ Tplus }
    '*'         { L _ Ttimes }
    '/'         { L _ Tdiv }
    '-.'        { L _ Tminusdot }
    '+.'        { L _ Tplusdot }
    '*.'        { L _ Tastdot }
    '/.'        { L _ Tslashdot }
    '='         { L _ Tequal }
    '<>'        { L _ Tnotequal }
    '<='        { L _ Tlessequal }
    '>='        { L _ Tgreaterequal }
    '<'         { L _ Tless }
    '>'         { L _ Tgreater }
    ','         { L _ Tcomma }
    '.'         { L _ Tdot }
    ';'         { L _ Tsemi }
    ';;'        { L _ Tsemisemi }
    '('         { L _ Tlparen }
    ')'         { L _ Trparen }
    '<-'        { L _ Tlessminus }

    'true'      { L _ Ttrue }
    'false'     { L _ Tfalse }
    'if'        { L _ Tif }
    'then'      { L _ Tthen }
    'else'      { L _ Telse }
    'let'       { L _ Tlet }
    'rec'       { L _ Trec }
    'in'        { L _ Tin }

    ident       { L _ (Tident _) }
    int         { L _ (Tint _) }
    float       { L _ (Tfloat _) }

    eof         { L _ Teof }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ Teof } 
%tokentype { Located Token }
%name parseExpr seq_expr
%name parse program

%nonassoc 'in'
%nonassoc ';;'
%nonassoc ';'
%nonassoc 'let'
%nonassoc 'then'
%nonassoc 'else'
%nonassoc '<-'
%nonassoc below_comma 
%left   ','
%nonassoc below_equal
%left   '=' '<>' '<' '>' '<=' '>='
%left   '+' '-' '+.' '-.'
%left   '*' '/' '*.' '/.'
%nonassoc   prec_unary_minus
%nonassoc   '.'
%%

program :: { [ LMcStmt ] }
    : stmt_tail            { $1 }
    | seq_expr stmt_tail   { sL (getLoc $1) (McEvalS $1) : $2 }

stmt_tail :: { [ LMcStmt ] }
    :                      { [] }
    | ';;'                  { [] }
    | ';;' seq_expr stmt_tail  { sL (getLoc $1) (McEvalS $2) : $3 }
    | ';;' stmt_item stmt_tail { $2 : $3 }
    | stmt_item stmt_tail     { $1 : $2 }

stmt_item :: { LMcStmt }
    : 'let' rec_flag let_binding
        { case unLoc $3 of
             (L _ McAnyP, exp) -> sL (comb2 $1 $>) (McEvalS exp)
             _ -> sL (comb2 $1 $>) (McValueS (unLoc $3))
        } 

val_ident ::    { Located Name }
    : ident             
        { sL (getLoc $1) (getIDENT $ $1) }

constant :: { Located Const }
    : int   { sL (getLoc $1) (IntC (getINT $1)) }
    | float { sL (getLoc $1) (FloatC (getFLOAT $1)) }
    | '(' ')' { sL (comb2 $1 $>) UnitC } 
    | 'true'  { sL (getLoc $1) (BoolC True) }
    | 'false' { sL (getLoc $1) (BoolC False) }


seq_expr :: { LMcExpr }
    : expr          { $1 }
    | expr ';'      { sL (comb2 $1 $>) (unLoc $1) }
    | expr ';' seq_expr         { sL (comb2 $1 $>) (McSeqE $1 $3) }

expr :: { LMcExpr }
    : simple_expr   { $1 }
    | simple_expr simple_expr_list 
        { sL (comb2 $1 $>) (McAppE $1 (reverse (unLoc $2))) }
    | 'let' rec_flag let_binding 'in' seq_expr
        { sL (comb2 $1 $>) (McLetE $2 (unLoc $3) $5) }
    | expr_comma_list
        { sL (getLoc $1) (McTupleE . reverse . unLoc $ $1) }
    | 'if' seq_expr 'then' expr 'else' expr
        { sL (comb2 $1 $>) (McCondE $2 $4 $6) }
    | '-' expr %prec prec_unary_minus
        { sL (comb2 $1 $>) (mkNeg $2) }
    | '-.' expr %prec prec_unary_minus
        { sL (comb2 $1 $>) (mkFNeg $2) }
    | expr '+' expr  { sL (comb2 $1 $>) (McInfixE Add $1 $3) }
    | expr '-' expr  { sL (comb2 $1 $>) (McInfixE Sub $1 $3) }
    | expr '*' expr  { sL (comb2 $1 $>) (McInfixE Mul $1 $3) }
    | expr '/' expr  { sL (comb2 $1 $>) (McInfixE Div $1 $3) }
    | expr '+.' expr { sL (comb2 $1 $>) (McInfixE FAdd $1 $3) }
    | expr '-.' expr { sL (comb2 $1 $>) (McInfixE FSub $1 $3) }
    | expr '*.' expr { sL (comb2 $1 $>) (McInfixE FMul $1 $3) }
    | expr '/.' expr { sL (comb2 $1 $>) (McInfixE FDiv $1 $3) }
    | expr '=' expr  { sL (comb2 $1 $>) (McInfixE Eq $1 $3) }
    | expr '<>' expr { sL (comb2 $1 $>) (McInfixE Ne $1 $3) }
    | expr '<=' expr { sL (comb2 $1 $>) (McInfixE Le $1 $3) }
    | expr '>=' expr { sL (comb2 $1 $>) (McInfixE Ge $1 $3) }
    | expr '<' expr  { sL (comb2 $1 $>) (McInfixE Lt $1 $3) }
    | expr '>' expr  { sL (comb2 $1 $>) (McInfixE Gt $1 $3) }
    | simple_expr '.' '(' seq_expr ')' '<-' expr
        { sL (comb2 $1 $>) 
            (McAppE (L noSrcLoc (McVarE $ mkName arraySetName)) [$1, $4, $7]) }

expr_comma_list :: { Located [LMcExpr] }
    : expr_comma_list ',' expr  { sL (comb2 $1 $>) ($3 : (unLoc $1)) }
    | expr ',' expr             { sL (comb2 $1 $>) [$3, $1] }

simple_expr :: { LMcExpr }
    : val_ident     { sL (getLoc $1) (McVarE (unLoc $1)) }
    | constant      { sL (getLoc $1) (McLitE (unLoc $1)) }
    | '(' seq_expr ')' { sL (comb2 $1 $>) (unLoc $2) }
    | simple_expr '.' '(' seq_expr ')'
        { sL (comb2 $1 $>) 
            (McAppE (L noSrcLoc (McVarE $ mkName arrayGetName)) [$1, $4]) }
        
simple_expr_list :: { Located [ LMcExpr ] }
    : simple_expr   { sL (getLoc $1) [ $1 ] }
    | simple_expr_list simple_expr { sL (comb2 $1 $>) ($2 : (unLoc $1)) }

rec_flag :: { RecFlag }
    :               { NonRec }
    | 'rec'         { Rec }


let_binding :: { Located (LMcPat, LMcExpr) }
    : val_ident fun_binding     { sL (comb2 $1 $>) (mkVarP $1, $2) }
    | pattern '=' seq_expr      { sL (comb2 $1 $>) ($1, $3) }

fun_binding :: { LMcExpr }
    : simple_pattern_list '=' seq_expr 
      { sL (comb2 $1 $>) (McFunE (reverse (unLoc $1)) $3) }

pattern :: { LMcPat }
    : simple_pattern    { $1 }
    | pattern_comma_list %prec below_comma
        { sL (getLoc $1) (McTupleP (reverse (unLoc $1))) }

simple_pattern :: { LMcPat }
    : val_ident %prec below_equal   { mkVarP $1 }
    | '_'                           { sL (getLoc $1) McAnyP }
    | '(' pattern ')'               { sL (comb2 $1 $>) (unLoc $2) }

simple_pattern_list :: { Located [LMcPat] }
    : simple_pattern_list simple_pattern
        { sL (comb2 $1 $>) ($2 : (unLoc $1)) }
    | simple_pattern 
        { sL (getLoc $1) [$1] }

pattern_comma_list :: { Located [LMcPat] }
    : pattern_comma_list ',' pattern    
        { sL (comb2 $1 $>) ($3 : unLoc $1) }
    | pattern ',' pattern               
        { sL (comb2 $1 $>) [$3, $1] }

{
getIDENT  (L _ (Tident x))  = x
getINT    (L _ (Tint x))    = x
getFLOAT  (L _ (Tfloat x))  = read x

mkVarE (L loc name) = sL loc (McVarE name)
mkVarP (L loc name) = sL loc (McVarP name)

mkNeg (L _ (McLitE (IntC n))) = McLitE (IntC (-n))
mkNeg (L _ (McLitE (FloatC f))) = McLitE (FloatC (-f))
mkNeg e = McPrefixE Neg e

mkFNeg (L _ (McLitE (FloatC f))) = McLitE (FloatC (-f))
mkFNeg e = McPrefixE FNeg e

reLoc (L loc _) (L _ e) = sL loc e

comb2 :: Located a -> Located b -> SrcLoc
comb2 a b = combineSrcLoc (getLoc a) (getLoc b)


happyError :: P a
happyError = fail "parse error"

{- external interface -}
parseProgram :: String -> ChocoM [LMcStmt]
parseProgram src = do
    case runP src parse of
        POk _ p -> return p
        PFailed loc msg -> 
            compileError (text msg) loc
}
