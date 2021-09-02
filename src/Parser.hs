module Parser where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.List

import Text.Parsec

import Front.Syntax
import Front.Literal
import Lexer

type Parser a = ParsecT Text.Text () Identity a

expression :: Parser Expr
expression = operator <|> term

operator :: Parser Expr
operator = do
    list <- many1 ((Left . Text.pack <$> operIdent) <|> (Right <$> term))
    case list of
        [Right expr] -> pure expr
        [Left oper] -> fail "Expected one or more operands"
        other -> (pure . EOper) other

term :: Parser Expr
term = value

value :: Parser Expr
value = (ELit <$> literal) <|> parens expression

literal :: Parser Literal
literal = try (LFloat <$> float) <|> integer

integer :: Parser Literal
integer = LInt <$> (decimal <|> octal <|> hexadecimal)

parse :: Text.Text -> Either String Expr
parse input =
    case runParser expression () "astral" input of
        Left err -> Left (show err)
        Right expr -> Right expr

