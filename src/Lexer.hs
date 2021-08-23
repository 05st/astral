module Lexer where

import Data.Text
import Data.Functor.Identity

import Text.Parsec
import qualified Text.Parsec.Token as Token

lexer :: Token.GenTokenParser Text () Identity
lexer = Token.makeTokenParser $ Token.LanguageDef
    { Token.commentStart = "{-"
    , Token.commentEnd = "-}"
    , Token.commentLine = "--"
    , Token.nestedComments = True
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.opStart = oneOf "!@#$%^&*-+=<>./?\\|~"
    , Token.opLetter = oneOf "!@#$%^&*-+=<>./?\\|~"
    , Token.reservedNames = ["let", "match", "with"]
    , Token.reservedOpNames = ["let", "match", "with"]
    , Token.caseSensitive = True
    }

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
operator = Token.operator lexer
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
