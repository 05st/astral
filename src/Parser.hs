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
expression = try application <|> operator <|> term

operator :: Parser Expr
operator = do
    list <- many1 ((Left . Text.pack <$> (whitespace *> operIdent <* whitespace)) <|> (Right <$> term))
    case list of
        [Right expr] -> pure expr
        [Left _] -> fail "Expected one or more operands"
        other -> (pure . EOper) other

application :: Parser Expr
application = do
    fn <- term
    calls <- many1 term
    pure (EApp fn calls)

term :: Parser Expr
term = value

value :: Parser Expr
value = (ELit <$> literal) <|> try variable <|> parens expression

variable :: Parser Expr
variable = EVar . Text.pack <$> (identifier <|> parens operIdent)

literal :: Parser Literal
literal = try (LFloat <$> float) <|> integer

integer :: Parser Literal
integer = LInt <$> (decimal <|> octal <|> hexadecimal)

parse :: Text.Text -> Either String Expr
parse input =
    case runParser expression () "astral" input of
        Left err -> Left (show err)
        Right expr -> Right expr

