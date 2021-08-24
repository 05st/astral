module Parser where

import Data.Text
import Data.Functor.Identity

import Text.Parsec

import Front.Syntax
import Front.Literal
import Lexer

type Parser a = ParsecT Text () Identity a

test :: Parser Expr
test = ELit . LChar <$> charLiteral
