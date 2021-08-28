module Parser where

import qualified Data.Text as Text
import Data.Functor.Identity
import Data.List

import Text.Parsec

import Front.Syntax
import Front.Literal
import Lexer

type Parser a = ParsecT Text.Text () Identity a

data Associativity
    = AssocLeft
    | AssocRight
    | AssocNone
    deriving (Eq)

data Fixity
    = FixPrefix
    | FixInfix Associativity
    | FixPostfix
    | FixClosed
    deriving (Eq)

type NamePart = String
type Arity = Int
data Operator = Operator Fixity [NamePart] deriving (Eq)

-- Takes an Operator and [Expr] and gives an EOper
operToExpr :: Operator -> [Expr] -> Expr
operToExpr (Operator FixClosed nps) = EOper (Text.pack (intercalate "_" nps))
operToExpr (Operator FixPrefix nps) = EOper (Text.pack (intercalate "_" nps ++ "_"))
operToExpr (Operator FixPostfix nps) = EOper (Text.pack ('_' : intercalate "_" nps))
operToExpr (Operator (FixInfix _) nps) = EOper (Text.pack ('_' : intercalate "_" nps ++ "_"))

-- The directed acyclic graph. Each node is a list of operators, an
-- edge from a to b indicates that the operators in b have a higher
-- precedence.
type Precedence = [Operator]
data PrecedenceGraph = PGraph [Operator] (Operator -> Precedence)

-- Inserting an edge onto the precedence graph, from a to b
addEdge :: PrecedenceGraph -> (Operator, Operator) -> PrecedenceGraph
addEdge (PGraph ops edges) (a, b) = PGraph ops edges'
    where edges' x
            | x == a = b : edges a
            | otherwise = edges x

-- Given a list of nameparts, and a specific parser for expressions, parse the expressions in between
closedOperands :: Parser Expr -> [NamePart] -> Parser [Expr]
closedOperands p (a : b : rest) = (:) <$> (spaces *> string a *> spaces *> p) <*> closedOperands p (b : rest)
closedOperands _ [a] = [] <$ (spaces *> string a *> spaces)
closedOperands _ [] = return []

-- Parses a 'tree' of operators.
operatorTree :: PrecedenceGraph -> Operator -> Parser Expr
operatorTree g@(PGraph _ edges) op@(Operator FixClosed nps) = operToExpr op <$> closedOperands (expression g) nps
operatorTree g@(PGraph _ edges) op@(Operator FixPrefix nps) = do
    closeds <- closedOperands (expression g) nps
    postExpr <- childExpr g (edges op) <|> operatorTree g op
    return $ operToExpr op (closeds ++ [postExpr])
operatorTree g@(PGraph _ edges) op@(Operator FixPostfix nps) = try $ do
    preExpr <- childExpr g (edges op)
    closeds <- closedOperands (expression g) nps
    return $ operToExpr op (preExpr : closeds)
operatorTree g@(PGraph _ edges) op@(Operator (FixInfix AssocLeft) nps) = try $ chainl1 (childExpr g (edges op)) (leftChain <$> closedOperands (expression g) nps)
    where leftChain args x y = operToExpr op (x : args ++ [y])
operatorTree g@(PGraph _ edges) op@(Operator (FixInfix AssocRight) nps) = try $ chainr1 (childExpr g (edges op)) (rightChain <$> closedOperands (expression g) nps)
    where rightChain args x y = operToExpr op (x : args ++ [y])

expression :: PrecedenceGraph -> Parser Expr
expression g@(PGraph ops _) = choice (map (operatorTree g) ops) <|> (ELit <$> literal)

childExpr :: PrecedenceGraph -> Precedence -> Parser Expr
childExpr g ops = choice (map (operatorTree g) ops) <|> (ELit <$> literal)

literal :: Parser Literal
literal = (LInt <$> decimal) <|> (LChar <$> charLiteral)

ifThenElse = Operator FixPrefix ["if", "then", "else"]
plusLeft = Operator (FixInfix AssocLeft) ["+"]
subRight = Operator (FixInfix AssocRight) ["-"]
access = Operator FixPostfix ["[", "]"]
parens = Operator FixClosed ["(", ")"]
exampleGraph = PGraph [ifThenElse, subRight, plusLeft, access, Parser.parens] (const [])
    `addEdge` (ifThenElse, Parser.parens)
    `addEdge` (ifThenElse, access)
    `addEdge` (access, Parser.parens)
    `addEdge` (ifThenElse, plusLeft)
    `addEdge` (plusLeft, access)
    `addEdge` (plusLeft, Parser.parens)
    `addEdge` (ifThenElse, subRight)

test :: Text.Text -> String
test input =
    case runParser (expression exampleGraph) () "test" input of 
        Left err -> error . show $ err
        Right e -> show e
