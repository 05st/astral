module Parser where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.List

import Text.Parsec

import Front.Syntax
import Front.Literal
import Front.Pattern
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
term = try lambda <|> value

lambda :: Parser Expr
lambda = do
    params <- many1 pattern
    reservedOp "=>"
    ELam params <$> expression

value :: Parser Expr
value = (ELit <$> try literal) <|> try variable <|> parens expression

variable :: Parser Expr
variable = EVar . Text.pack <$> (identifier <|> parens operIdent)

literal :: Parser Literal
literal = try (LFloat <$> float) <|> integer

integer :: Parser Literal
integer = LInt <$> (decimal <|> octal <|> hexadecimal)

pattern :: Parser Pattern
pattern = parens pattern <|> patternWild <|> try patternAs <|> patternCon <|> patternVar <|> patternLit

patternWild :: Parser Pattern
patternWild = PWild <$ reservedOp "_"

patternVar :: Parser Pattern
patternVar = PVar . Text.pack <$> identifier

patternAs :: Parser Pattern
patternAs = do
    var <- identifier
    reservedOp "@"
    PAs (Text.pack var) <$> pattern

patternCon :: Parser Pattern
patternCon = do
    ident <- dataIdent
    PCon (Text.pack ident) <$> many pattern

patternLit :: Parser Pattern
patternLit = PLit <$> literal

parse :: Text.Text -> Either String Expr
parse input =
    case runParser expression () "astral" input of
        Left err -> Left (show err)
        Right expr -> Right expr

