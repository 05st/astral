module Main where

import qualified Data.Text as Text

import Parser

main :: IO ()
main = getLine >>= putStrLn . test . Text.pack >> main
