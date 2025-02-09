-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParGramatyka
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified AbsGramatyka
import LexGramatyka

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'        { PT _ (TS _ 1)  }
  '!='       { PT _ (TS _ 2)  }
  '%'        { PT _ (TS _ 3)  }
  '&'        { PT _ (TS _ 4)  }
  '&&'       { PT _ (TS _ 5)  }
  '('        { PT _ (TS _ 6)  }
  ')'        { PT _ (TS _ 7)  }
  '*'        { PT _ (TS _ 8)  }
  '+'        { PT _ (TS _ 9)  }
  '++'       { PT _ (TS _ 10) }
  ','        { PT _ (TS _ 11) }
  '-'        { PT _ (TS _ 12) }
  '--'       { PT _ (TS _ 13) }
  '/'        { PT _ (TS _ 14) }
  ';'        { PT _ (TS _ 15) }
  '<'        { PT _ (TS _ 16) }
  '<='       { PT _ (TS _ 17) }
  '='        { PT _ (TS _ 18) }
  '=='       { PT _ (TS _ 19) }
  '>'        { PT _ (TS _ 20) }
  '>='       { PT _ (TS _ 21) }
  'boolean'  { PT _ (TS _ 22) }
  'break'    { PT _ (TS _ 23) }
  'continue' { PT _ (TS _ 24) }
  'else'     { PT _ (TS _ 25) }
  'false'    { PT _ (TS _ 26) }
  'if'       { PT _ (TS _ 27) }
  'int'      { PT _ (TS _ 28) }
  'print'    { PT _ (TS _ 29) }
  'return'   { PT _ (TS _ 30) }
  'string'   { PT _ (TS _ 31) }
  'true'     { PT _ (TS _ 32) }
  'void'     { PT _ (TS _ 33) }
  'while'    { PT _ (TS _ 34) }
  '{'        { PT _ (TS _ 35) }
  '||'       { PT _ (TS _ 36) }
  '}'        { PT _ (TS _ 37) }
  L_Ident    { PT _ (TV _)    }
  L_integ    { PT _ (TI _)    }
  L_quoted   { PT _ (TL _)    }

%%

Ident :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Ident) }
Ident  : L_Ident { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Ident (tokenText $1)) }

Integer :: { (AbsGramatyka.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (AbsGramatyka.BNFC'Position, String) }
String   : L_quoted { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Program) }
Program
  : ListTopDef { (fst $1, AbsGramatyka.Program (fst $1) (snd $1)) }

TopDef :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.TopDef) }
TopDef
  : FunType Ident '(' ListArg ')' Block { (fst $1, AbsGramatyka.FnDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }

ListTopDef :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.TopDef]) }
ListTopDef
  : TopDef { (fst $1, (:[]) (snd $1)) }
  | TopDef ListTopDef { (fst $1, (:) (snd $1) (snd $2)) }

Arg :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Arg) }
Arg
  : ArgType Ident { (fst $1, AbsGramatyka.Arg (fst $1) (snd $1) (snd $2)) }

ListArg :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.Arg]) }
ListArg
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Block :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Block) }
Block
  : '{' ListStmt '}' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Block (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.Stmt]) }
ListStmt
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Stmt) }
Stmt
  : ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Empty (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, AbsGramatyka.BStmt (fst $1) (snd $1)) }
  | Type ListItem ';' { (fst $1, AbsGramatyka.Decl (fst $1) (snd $1) (snd $2)) }
  | Ident '=' Expr ';' { (fst $1, AbsGramatyka.Ass (fst $1) (snd $1) (snd $3)) }
  | Ident '++' ';' { (fst $1, AbsGramatyka.Incr (fst $1) (snd $1)) }
  | Ident '--' ';' { (fst $1, AbsGramatyka.Decr (fst $1) (snd $1)) }
  | 'return' Expr ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Ret (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.VRet (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr ')' Block { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Cond (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Block 'else' Block { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.CondElse (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Block { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.While (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | Expr ';' { (fst $1, AbsGramatyka.SExp (fst $1) (snd $1)) }
  | 'continue' ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Cont (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'break' ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Break (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'print' Expr ';' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Print (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }

Item :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Item) }
Item
  : Ident { (fst $1, AbsGramatyka.NoInit (fst $1) (snd $1)) }
  | Ident '=' Expr { (fst $1, AbsGramatyka.Init (fst $1) (snd $1) (snd $3)) }

ListItem :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.Item]) }
ListItem
  : Item { (fst $1, (:[]) (snd $1)) }
  | Item ',' ListItem { (fst $1, (:) (snd $1) (snd $3)) }

Type :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Type) }
Type
  : 'int' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Int (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Str (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'boolean' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Bool (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }

ListType :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.Type]) }
ListType
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }
  | {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }

FunType :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.FunType) }
FunType
  : 'void' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Void (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | Type { (fst $1, AbsGramatyka.RetType (fst $1) (snd $1)) }

ArgType :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.ArgType) }
ArgType
  : Type '*' { (fst $1, AbsGramatyka.Ref (fst $1) (snd $1)) }
  | Type { (fst $1, AbsGramatyka.Value (fst $1) (snd $1)) }

ListArgType :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.ArgType]) }
ListArgType
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | ArgType { (fst $1, (:[]) (snd $1)) }
  | ArgType ',' ListArgType { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr6
  : Ident { (fst $1, AbsGramatyka.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, AbsGramatyka.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.ELitTrue (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.ELitFalse (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListPassedArg ')' { (fst $1, AbsGramatyka.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, AbsGramatyka.EString (fst $1) (snd $1)) }
  | '(' Expr ')' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr5
  : '-' Expr6 { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Neg (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Not (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, AbsGramatyka.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, AbsGramatyka.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, AbsGramatyka.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, AbsGramatyka.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, AbsGramatyka.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.Expr]) }
ListExpr
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

PassedArg :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.PassedArg) }
PassedArg
  : Expr { (fst $1, AbsGramatyka.EVal (fst $1) (snd $1)) }
  | '&' Ident { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.ERef (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListPassedArg :: { (AbsGramatyka.BNFC'Position, [AbsGramatyka.PassedArg]) }
ListPassedArg
  : {- empty -} { (AbsGramatyka.BNFC'NoPosition, []) }
  | PassedArg { (fst $1, (:[]) (snd $1)) }
  | PassedArg ',' ListPassedArg { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.AddOp) }
AddOp
  : '+' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Plus (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Minus (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.MulOp) }
MulOp
  : '*' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Times (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Div (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.Mod (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (AbsGramatyka.BNFC'Position, AbsGramatyka.RelOp) }
RelOp
  : '<' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.LTH (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.LE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.GTH (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.GE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.EQU (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1), AbsGramatyka.NE (uncurry AbsGramatyka.BNFC'Position (tokenLineCol $1))) }

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

pProgram :: [Token] -> Err AbsGramatyka.Program
pProgram = fmap snd . pProgram_internal
}

