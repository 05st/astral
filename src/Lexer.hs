module Lexer where

import Data.Text
import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Token as Token

tokenDef = Token.LanguageDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.commentLine = "--"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.opStart = oneOf "!@#$%^&*-+=<>./?\\|~"
    , Token.opLetter = oneOf "!@#$%^&*-+=<>./?\\|~"
    , Token.reservedNames = ["let", "in", "match", "with", "if", "then", "else", "True", "False", "()"]
    , Token.reservedOpNames = ["->", "=>", "_", "@", "\\"]
    , Token.caseSensitive = True
    }

lexer :: Token.GenTokenParser Text () Identity
lexer = Token.makeTokenParser tokenDef

dataLexer :: Token.GenTokenParser Text () Identity
dataLexer = Token.makeTokenParser $ tokenDef { Token.identStart = upper }

identifier = Token.identifier lexer
dataIdent = Token.identifier dataLexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operIdent = Token.operator lexer
parens = Token.parens lexer
decimal = Token.decimal lexer
octal = Token.octal lexer
hexadecimal = Token.hexadecimal lexer
float = Token.float lexer
semi = Token.semi lexer
colon = Token.colon lexer
whitespace = Token.whiteSpace lexer
braces = Token.braces lexer
comma = Token.comma lexer
dot = Token.dot lexer
angles = Token.angles lexer
brackets = Token.brackets lexer
charLiteral = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer
