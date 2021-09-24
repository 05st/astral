{-# Language TupleSections #-}

module Parser where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.List

import Text.Parsec

import Front.Syntax
import Front.Literal
import Front.Pattern
import Front.Type
import Lexer

type Parser a = ParsecT Text.Text () Identity a

declaration :: Parser Decl
declaration = letDecl

letDecl :: Parser Decl
letDecl = do
    reserved "let"
    var <- identifier
    sigMaybe <- optionMaybe typeAnnot
    (reservedOp "=" *> (DLet (Text.pack var) sigMaybe <$> expression) <* semi)
        <|> (DLetFn (Text.pack var) sigMaybe <$> (braces (many1 letBranch) <|> many1 letBranch))
    where
        letBranch = do
            patterns <- many1 pattern
            reservedOp "="
            expr <- expression
            semi
            pure (patterns, expr) 

expression :: Parser Expr
expression = try application <|> try operator <|> term

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
term = try lambda <|> match <|> ifExpr <|> letExpr <|> value

lambda :: Parser Expr
lambda = do
    params <- many1 pattern
    reservedOp "=>"
    ELam params <$> expression

letExpr :: Parser Expr
letExpr = do
    reserved "let"
    var <- identifier
    sigMaybe <- optionMaybe typeAnnot
    reservedOp "="
    binding <- expression
    whitespace
    reserved "in"
    ELet (Text.pack var) sigMaybe binding <$> expression

match :: Parser Expr
match = do
    reserved "match"
    mexpr <- expression
    reserved "with"
    EMatch mexpr <$> sepBy1 matchBranch comma
    where
        matchBranch = do
            pat <- pattern
            reservedOp "->"
            (pat,) <$> expression

ifExpr :: Parser Expr
ifExpr = do
    reserved "if"
    cexpr <- expression
    reserved "then"
    expr <- expression
    reserved "else"
    EIf cexpr expr <$> expression

value :: Parser Expr
value = (ELit <$> try literal) <|> list <|> try variable <|> parens expression

list :: Parser Expr
list = EList <$> brackets (sepBy expression comma)

variable :: Parser Expr
variable = EVar . Text.pack <$> (identifier <|> parens operIdent)

literal :: Parser Literal
literal = try (LFloat <$> float) <|> integer <|> bool <|> (LUnit <$ reserved "()") <|> (LChar <$> charLiteral) <|> (LString <$> stringLiteral)

integer :: Parser Literal
integer = LInt <$> (decimal <|> octal <|> hexadecimal)

bool :: Parser Literal
bool = (LBool True <$ reserved "True") <|> (LBool False <$ reserved "False")

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
patternLit = PLit <$> literal <* whitespace

typeAnnot :: Parser Type
typeAnnot = reservedOp ":" *> type'

type' :: Parser Type
type' = try typeFunc <|> try typeApp <|> typeBase

typeFunc :: Parser Type
typeFunc = do
    inputType <- try typeApp <|> typeBase
    reservedOp "->"
    (inputType :->) <$> type'

typeApp :: Parser Type
typeApp = (:@:) <$> typeBase <*> type'

typeBase :: Parser Type
typeBase = (flip TCon None . Text.pack <$> dataIdent) <|> typeVar <|> parens type'

typeVar :: Parser Type
typeVar = flip TVar None . Text.pack <$> identifier

parse :: Text.Text -> Either String [Decl]
parse input =
    case runParser (many declaration) () "astral" input of
        Left err -> Left (show err)
        Right decls -> Right decls
