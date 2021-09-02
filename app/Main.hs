module Main where

import qualified Data.Text as Text

import Parser

main :: IO ()
main = getLine >>= runParse . Text.pack >> main
    where
        runParse input =
            case parse input of
                Left err -> putStrLn err
                Right expr -> print expr

