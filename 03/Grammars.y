{
module Grammars where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      id              { TokenID $$ }
      int             { TokenNum $$ }
      bool            { TokenBool $$ }
      op              { TokenOp $$ }
      let             { TokenLet }
      lets            { TokenLetStar }
      '('             { TokenPA }
      ')'             { TokenPC }
      '['             { TokenCA }
      ']'             { TokenCC }

%%

ASAS : id                                   { IdS $1 }
     | int                                  { NumS $1 }
     | bool                                 { BooleanS $1 }
     | '(' op LASAS ')'                     { OpS $2 $3 }
     | '(' let '(' LBIND ')' ASAS ')'       { LetS $4 $6 }
     | '(' lets '(' LBIND ')' ASAS ')'      { LetStar $4 $6 }

LASAS : ASAS           { [$1] }
      | ASAS LASAS     { $1:$2 }

BIND : '[' id ASAS ']'  { ($2,$3) }

LBIND : BIND         { [$1] }
      | BIND LBIND   { $1:$2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

type BindingS = (String,ASAS)

data ASAS = IdS String
          | NumS Int
          | BooleanS Bool
          | OpS String [ASAS]
          | LetS [BindingS] ASAS
          | LetStar [BindingS] ASAS
          deriving(Show)

data Token = TokenID String
           | TokenNum Int
           | TokenBool Bool
           | TokenOp String
           | TokenLet
           | TokenLetStar
           | TokenPA
           | TokenPC
           | TokenCA
           | TokenCC
           deriving(Show)

lexer :: String -> [Token]
lexer [] = []
lexer (' ':xs) = lexer xs
lexer ('(':xs) = TokenPA:(lexer xs)
lexer (')':xs) = TokenPC:(lexer xs)
lexer ('[':xs) = TokenCA:(lexer xs)
lexer (']':xs) = TokenCC:(lexer xs)
lexer ('+':xs) = (TokenOp "+"):(lexer xs)
lexer ('-':xs) = (TokenOp "-"):(lexer xs)
lexer ('*':xs) = (TokenOp "*"):(lexer xs)
lexer ('/':xs) = (TokenOp "/"):(lexer xs)
lexer ('<':xs) = (TokenOp "<"):(lexer xs)
lexer ('>':xs) = (TokenOp ">"):(lexer xs)
lexer ('=':xs) = (TokenOp "="):(lexer xs)
lexer ('a':'d':'d':'1':xs) = (TokenOp "add1"):(lexer xs)
lexer ('s':'u':'b':'1':xs) = (TokenOp "sub1"):(lexer xs)
lexer ('n':'o':'t':xs) = (TokenOp "not"):(lexer xs)
lexer ('a':'n':'d':xs) = (TokenOp "and"):(lexer xs)
lexer ('o':'r':xs) = (TokenOp "or"):(lexer xs)
lexer ('#':'t':xs) = (TokenBool True):(lexer xs)
lexer ('#':'f':xs) = (TokenBool False):(lexer xs)
lexer ('l':'e':'t':'*':xs) = TokenLetStar:(lexer xs)
lexer ('l':'e':'t':xs) = TokenLet:(lexer xs)
lexer (x:xs)
        | isDigit x = lexNum (x:xs)
        | isAlpha x = lexAlph (x:xs)

lexNum :: String -> [Token]
lexNum cs = TokenNum (read num) : lexer rest
                    where (num,rest) = span isDigit cs

lexAlph :: String -> [Token]
lexAlph cs = (TokenID var):lexer rest
               where (var,rest) = span isAlpha cs

main = getContents >>= print . parse . lexer

}
