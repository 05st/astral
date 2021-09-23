module Main where

import qualified Data.Text as Text
import System.Environment

import Parser

main :: IO ()
main = do
    file <- head <$> getArgs
    readFile file >>= runParse . Text.pack
    where
        runParse input =
            case parse input of
                Left err -> putStrLn err
                Right decl -> print decl
