-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.3).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParLatte
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsLatte
import LexLatte

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'       { PT _ (TS _ 1)  }
  '!='      { PT _ (TS _ 2)  }
  '%'       { PT _ (TS _ 3)  }
  '&&'      { PT _ (TS _ 4)  }
  '('       { PT _ (TS _ 5)  }
  ')'       { PT _ (TS _ 6)  }
  '*'       { PT _ (TS _ 7)  }
  '+'       { PT _ (TS _ 8)  }
  '++'      { PT _ (TS _ 9)  }
  ','       { PT _ (TS _ 10) }
  '-'       { PT _ (TS _ 11) }
  '--'      { PT _ (TS _ 12) }
  '.'       { PT _ (TS _ 13) }
  '/'       { PT _ (TS _ 14) }
  ':'       { PT _ (TS _ 15) }
  ';'       { PT _ (TS _ 16) }
  '<'       { PT _ (TS _ 17) }
  '<='      { PT _ (TS _ 18) }
  '='       { PT _ (TS _ 19) }
  '=='      { PT _ (TS _ 20) }
  '>'       { PT _ (TS _ 21) }
  '>='      { PT _ (TS _ 22) }
  '['       { PT _ (TS _ 23) }
  '[]'      { PT _ (TS _ 24) }
  ']'       { PT _ (TS _ 25) }
  'boolean' { PT _ (TS _ 26) }
  'class'   { PT _ (TS _ 27) }
  'else'    { PT _ (TS _ 28) }
  'extends' { PT _ (TS _ 29) }
  'false'   { PT _ (TS _ 30) }
  'for'     { PT _ (TS _ 31) }
  'if'      { PT _ (TS _ 32) }
  'int'     { PT _ (TS _ 33) }
  'new'     { PT _ (TS _ 34) }
  'null'    { PT _ (TS _ 35) }
  'return'  { PT _ (TS _ 36) }
  'string'  { PT _ (TS _ 37) }
  'true'    { PT _ (TS _ 38) }
  'void'    { PT _ (TS _ 39) }
  'while'   { PT _ (TS _ 40) }
  '{'       { PT _ (TS _ 41) }
  '||'      { PT _ (TS _ 42) }
  '}'       { PT _ (TS _ 43) }
  L_Ident   { PT _ (TV _)    }
  L_integ   { PT _ (TI _)    }
  L_quoted  { PT _ (TL _)    }

%%

Ident :: { (AbsLatte.BNFC'Position, AbsLatte.Ident) }
Ident  : L_Ident { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Ident (tokenText $1)) }

Integer :: { (AbsLatte.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (AbsLatte.BNFC'Position, String) }
String   : L_quoted { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (AbsLatte.BNFC'Position, AbsLatte.Program) }
Program : ListTopDef { (fst $1, AbsLatte.Prog (fst $1) (snd $1)) }

TopDef :: { (AbsLatte.BNFC'Position, AbsLatte.TopDef) }
TopDef
  : Type Ident '(' ListArg ')' Block { (fst $1, AbsLatte.FnDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }
  | 'class' Ident '{' ListClassProp '}' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ClassDef (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | 'class' Ident 'extends' Ident '{' ListClassProp '}' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ClassExtDef (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4) (snd $6)) }

ClassProp :: { (AbsLatte.BNFC'Position, AbsLatte.ClassProp) }
ClassProp
  : Type Ident ';' { (fst $1, AbsLatte.CAttribute (fst $1) (snd $1) (snd $2)) }
  | Type Ident '(' ListArg ')' Block { (fst $1, AbsLatte.CMethod (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }

ListTopDef :: { (AbsLatte.BNFC'Position, [AbsLatte.TopDef]) }
ListTopDef
  : TopDef { (fst $1, (:[]) (snd $1)) }
  | TopDef ListTopDef { (fst $1, (:) (snd $1) (snd $2)) }

Arg :: { (AbsLatte.BNFC'Position, AbsLatte.Arg) }
Arg
  : Type Ident { (fst $1, AbsLatte.Arg (fst $1) (snd $1) (snd $2)) }

ListArg :: { (AbsLatte.BNFC'Position, [AbsLatte.Arg]) }
ListArg
  : {- empty -} { (AbsLatte.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

ListClassProp :: { (AbsLatte.BNFC'Position, [AbsLatte.ClassProp]) }
ListClassProp
  : ClassProp { (fst $1, (:[]) (snd $1)) }
  | ClassProp ListClassProp { (fst $1, (:) (snd $1) (snd $2)) }

Block :: { (AbsLatte.BNFC'Position, AbsLatte.Block) }
Block
  : '{' ListStmt '}' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Block (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (AbsLatte.BNFC'Position, [AbsLatte.Stmt]) }
ListStmt
  : {- empty -} { (AbsLatte.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (AbsLatte.BNFC'Position, AbsLatte.Stmt) }
Stmt
  : ';' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Empty (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, AbsLatte.BStmt (fst $1) (snd $1)) }
  | Type ListItem ';' { (fst $1, AbsLatte.Decl (fst $1) (snd $1) (snd $2)) }
  | Expr '=' Expr ';' { (fst $1, AbsLatte.Ass (fst $1) (snd $1) (snd $3)) }
  | Ident '++' ';' { (fst $1, AbsLatte.Incr (fst $1) (snd $1)) }
  | Ident '--' ';' { (fst $1, AbsLatte.Decr (fst $1) (snd $1)) }
  | 'return' Expr ';' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Ret (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.VRet (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr ')' Stmt { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Cond (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Stmt 'else' Stmt { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.CondElse (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Stmt { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.While (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'for' '(' Type Ident ':' Expr ')' Stmt { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.For (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $4) (snd $6) (snd $8)) }
  | Expr ';' { (fst $1, AbsLatte.SExp (fst $1) (snd $1)) }

Item :: { (AbsLatte.BNFC'Position, AbsLatte.Item) }
Item
  : Ident { (fst $1, AbsLatte.NoInit (fst $1) (snd $1)) }
  | Ident '=' Expr { (fst $1, AbsLatte.Init (fst $1) (snd $1) (snd $3)) }

ListItem :: { (AbsLatte.BNFC'Position, [AbsLatte.Item]) }
ListItem
  : Item { (fst $1, (:[]) (snd $1)) }
  | Item ',' ListItem { (fst $1, (:) (snd $1) (snd $3)) }

Type :: { (AbsLatte.BNFC'Position, AbsLatte.Type) }
Type
  : 'int' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Int (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Str (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | 'boolean' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Bool (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | 'void' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Void (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | Type '[]' { (fst $1, AbsLatte.Arr (fst $1) (snd $1)) }
  | Ident { (fst $1, AbsLatte.Class (fst $1) (snd $1)) }

ListType :: { (AbsLatte.BNFC'Position, [AbsLatte.Type]) }
ListType
  : {- empty -} { (AbsLatte.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr6
  : Ident { (fst $1, AbsLatte.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, AbsLatte.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ELitTrue (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ELitFalse (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, AbsLatte.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, AbsLatte.EString (fst $1) (snd $1)) }
  | Expr6 '.' Ident { (fst $1, AbsLatte.EAtr (fst $1) (snd $1) (snd $3)) }
  | Expr6 '.' Ident '(' ListExpr ')' { (fst $1, AbsLatte.ECMethodApp (fst $1) (snd $1) (snd $3) (snd $5)) }
  | '(' Expr ')' 'null' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ENull (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'new' Type '[' Expr ']' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.EArr (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2) (snd $4)) }
  | Expr6 '[' Expr ']' { (fst $1, AbsLatte.EArrGet (fst $1) (snd $1) (snd $3)) }
  | '(' Expr ')' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr5
  : 'new' Ident { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.ENew (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '-' Expr6 { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Neg (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Not (uncurry AbsLatte.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, AbsLatte.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, AbsLatte.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, AbsLatte.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, AbsLatte.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (AbsLatte.BNFC'Position, AbsLatte.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, AbsLatte.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (AbsLatte.BNFC'Position, [AbsLatte.Expr]) }
ListExpr
  : {- empty -} { (AbsLatte.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (AbsLatte.BNFC'Position, AbsLatte.AddOp) }
AddOp
  : '+' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Plus (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Minus (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (AbsLatte.BNFC'Position, AbsLatte.MulOp) }
MulOp
  : '*' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Times (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Div (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.Mod (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (AbsLatte.BNFC'Position, AbsLatte.RelOp) }
RelOp
  : '<' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.LTH (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.LE (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.GTH (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.GE (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.EQU (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry AbsLatte.BNFC'Position (tokenLineCol $1), AbsLatte.NE (uncurry AbsLatte.BNFC'Position (tokenLineCol $1))) }

{

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

-- Entrypoints

pProgram :: [Token] -> Err AbsLatte.Program
pProgram = fmap snd . pProgram_internal
}

