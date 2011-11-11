{
{-# OPTIONS_GHC -w #-}
------------------------------------------------- 
--                   Choco                     --
--         Chikadzume Oriented Compiler        --
--    Copyright 2007-2008 by Basement fairy    --
-------------------------------------------------
module LibParse (parseLib) where

import Choco
import Const
import LamSyn
import LibLex
import Primitive
import SrcLoc
import Var

import Control.Monad
import qualified Data.Map as M

{- The parser for library -}
}

%token
    '='     { L _ Tequal }
    '-'     { L _ Tminus }
    '+'     { L _ Tplus }
    '*'     { L _ Ttimes }
    '/'     { L _ Tdiv }
    '-.'    { L _ Tminusdot }
    '+.'    { L _ Tplusdot }
    '*.'    { L _ Tastdot }
    '/.'    { L _ Tslashdot }
    '=='    { L _ Tequalequal }
    '<>'    { L _ Tnotequal }
    '<='    { L _ Tlessequal }
    '>='    { L _ Tgreaterequal }
    '<'     { L _ Tless }
    '>'     { L _ Tgreater }
    '==.'   { L _ Tequalequaldot }
    '<>.'   { L _ Tnotequaldot }
    '<=.'   { L _ Tlessequaldot }
    '>=.'   { L _ Tgreaterequaldot }
    '<.'    { L _ Tlessdot }
    '>.'    { L _ Tgreaterdot }
    ';'     { L _ Tsemi }
    '('     { L _ Tlparen }
    ')'     { L _ Trparen }

    'function'  { L _ Tfunction }
    'call'      { L _ Tcall }
    'if'        { L _ Tif }
    'let_const' { L _ Tlet_const }
    'in'        { L _ Tin }

    ident   { L _ (Tident _) }
    int     { L _ (Tint _) }
    float   { L _ (Tfloat _) }

    eof     { L _ Teof }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ Teof } 
%tokentype { Located Token }
%name parse fundecls

%nonassoc 'let_const'
%nonassoc ';'
%left '==' '<>' '<' '>' '<=' '>=' '==.' '<>.' '<=.' '>=.' '<.' '>.'
%left '+' '-' '+.' '-.'
%left '*' '/' '*.' '/.'
%nonassoc  prec_unary_minus
%%

fundecls :: { [ (Var, Lambda) ] }
    :                      { [] }
    | fundecls fundecl     { $2 : $1 }

fundecl :: { (Var, Lambda) }
    : 'function' ident '(' params ')' '(' lam ')'
                { (makeVar $2, Lfun (reverse $4) $7 ) }

params :: { [Var] }
    :                { [] }
    | params ident   { makeVar $2 : $1 }


lam :: { Lambda }
    : simple_lam      { $1 }
    | simple_lam simple_lam_list { makeApp $1 (reverse $2) }
    | 'let_const' ident '=' lam 'in' lam  
        { Llet Strict (makeVar $2, $4) $6 }
    | 'if' simple_lam simple_lam simple_lam { Lcond $2 $3 $4 }
    | '-' lam %prec prec_unary_minus  { makeNeg $2 }
    | '-.' lam %prec prec_unary_minus { makeFNeg $2 }

constant :: { Const }
    : int          { makeIntC $1 }
    | float        { makeFloatC $1 }

simple_lam :: { Lambda }
    : ident         { Lvar (makeVar $1) }
    | constant      { Llit $1 }
    | '(' seq_lam ')'    { $2 }

simple_lam_list :: { [Lambda] }
    : simple_lam    { [$1] }
    | simple_lam_list simple_lam { $2 : $1 }

seq_lam :: { Lambda }
    : lam        { $1 }
    | lam ';'    { $1 }
    | lam ';' seq_lam { Lseq $1 $3 }
    | lam '+' lam       { Lprim Paddi [$1, $3] }
    | lam '-' lam       { Lprim Psubi [$1, $3] }
    | lam '*' lam       { Lprim Pmuli [$1, $3] }
    | lam '/' lam       { Lprim Pdivi [$1, $3] }
    | lam '+.' lam      { Lprim Paddf [$1, $3] }
    | lam '-.' lam      { Lprim Psubf [$1, $3] }
    | lam '*.' lam      { Lprim Pmulf [$1, $3] }
    | lam '/.' lam      { Lprim Pdivf [$1, $3] }
    | lam '==' lam      { Lprim (Pcompi Ceq) [$1, $3] }
    | lam '<>' lam      { Lprim (Pcompi Cne) [$1, $3] }
    | lam '<'  lam      { Lprim (Pcompi Clt) [$1, $3] }
    | lam '>'  lam      { Lprim (Pcompi Cgt) [$1, $3] }
    | lam '<=' lam      { Lprim (Pcompi Cle) [$1, $3] }
    | lam '>=' lam      { Lprim (Pcompi Cge) [$1, $3] }
    | lam '==.' lam     { Lprim (Pcompf Ceq) [$1, $3] }
    | lam '<>.' lam     { Lprim (Pcompf Cne) [$1, $3] }
    | lam '<.'  lam     { Lprim (Pcompf Clt) [$1, $3] }
    | lam '>.'  lam     { Lprim (Pcompf Cgt) [$1, $3] }
    | lam '<=.' lam     { Lprim (Pcompf Cle) [$1, $3] }
    | lam '>=.' lam     { Lprim (Pcompf Cge) [$1, $3] }


{
makeVar (L _ (Tident x))    = Var{ var_name = x }
makeIntC (L _ (Tint n))     = IntC n
makeFloatC (L _ (Tfloat f)) = FloatC (read f)

makeApp (Lvar f) args = 
    let (_, lm) = primTable in
    case M.lookup (var_name f) lm of
        Just fn -> fn args
        Nothing -> Lapp (Lvar f) args True

makeNeg (Llit (IntC n)) = Llit (IntC (-n))
makeNeg e = Lprim Pnegi [e]

makeFNeg (Llit (FloatC f)) = Llit (FloatC (-f))
makeFNeg e = Lprim Pnegf [e]

happyError :: P a
happyError = fail "parse error"

{- external interface -}
parseLib :: String -> Lambda -> ChocoM Lambda
parseLib src cont = do
    case runP src parse of
        POk _ fundecls -> do
            runIO$print$ fundecls
            fundecls' <- mapM t' ident '=' lam 'in' lam  
                    { Llet Strict (makeVar $2, $3) $4 }
                    ename fundecls
            return $ foldr (\(v, l) cont -> Llet Strict (v, l) cont)
                        cont fundecls'
        PFailed loc msg ->
            compileError (text msg) loc
    where
    rename (v, Lfun params body)
        = do vid <- newUniq 
             paramsid <- replicateM (length params) newUniq
             let tbl = M.fromList $ zip (map var_name (v:params)) (vid:paramsid)
             body' <- walk tbl body
             let v' = v{ var_id = vid }
             let params' = zipWith (\p i -> p{ var_id = i }) params paramsid
             return (v', Lfun params' body')
    walk m (Lvar v) = return $ Lvar v
    walk m lam = return lam
}
