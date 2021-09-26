{-# Language TupleSections #-}

module Parser (Parser.parse) where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.List
import Data.Function

import Control.Monad.Except

import Text.Parsec
import Text.Parsec.Expr

import Base.Literal
import Base.Pattern
import Base.Type
import Syntax

import Lexer
import Monad

type Parser a = ParsecT Text.Text [OperatorDef] Identity a

declaration :: Parser Decl
declaration = letDecl <|> operDecl

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

operDecl :: Parser Decl
operDecl = (parseOperDecl "infixl" ALeft <|> parseOperDecl "infixr" ARight <|> parseOperDecl "infix" ANone
        <|> parseOperDecl "prefix" APrefix <|> parseOperDecl "postfix" APostfix) <* semi
    where
        addOperator op = modifyState (op :)
        parseOperDecl str assoc = do
            reserved str
            prec <- decimal
            whitespace
            oper <- Text.pack <$> operIdent
            let op = OperatorDef assoc prec oper
            addOperator op
            pure (DOper op)

expression :: Parser Expr
expression = foldl1 EApp <$> many1 operator

operator :: Parser Expr
operator = do
    opers <- getState 
    let table = mkTable opers
    buildExpressionParser table term
    where
        mkTable ops = map (map toParser) . groupBy ((==) `on` prec) . sortBy (flip compare `on` prec) $ ops
        toParser (OperatorDef assoc _ oper) = case assoc of
            ALeft -> infixOp oper (EBinOp oper) (toAssoc assoc)
            ARight -> infixOp oper (EBinOp oper) (toAssoc assoc)
            ANone -> infixOp oper (EBinOp oper) (toAssoc assoc)
            APrefix -> prefixOp oper (EUnaOp oper)
            APostfix -> postfixOp oper (EUnaOp oper)
        infixOp name f = Infix (reservedOp (Text.unpack name) >> return f)
        prefixOp name f = Prefix (reservedOp (Text.unpack name) >> return f)
        postfixOp name f = Postfix (reservedOp (Text.unpack name) >> return f)
        toAssoc ALeft = AssocLeft
        toAssoc ARight = AssocRight
        toAssoc ANone = AssocNone

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
value = (ELit <$> try literal) <|> list <|> stringLit <|> try variable <|> parens expression

list :: Parser Expr
list = EList <$> brackets (sepBy expression comma)

stringLit :: Parser Expr
stringLit = EString <$> stringLiteral

variable :: Parser Expr
variable = EVar . Text.pack <$> (identifier <|> parens operIdent)

literal :: Parser Literal
literal = try (LFloat <$> float) <|> integer <|> bool <|> (LUnit <$ reserved "()") <|> (LChar <$> charLiteral)

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

parse :: Text.Text -> Astral [Decl]
parse input =
    case runParser (many1 declaration) [] "astral" input of
        Left err -> throwError (show err)
        Right decls -> pure decls
