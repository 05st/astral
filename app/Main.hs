{-# Language LambdaCase #-}

module Main where

import qualified Data.Text as Text
import System.Environment

import Control.Monad

import Parser
import Monad

compile = parse

main :: IO ()
main = do
    file <- head <$> getArgs
    readFile file >>= runParse . Text.pack
    where
        runParse input = runAstral (compile input) emptyState >>= \case
            (Left err, state) -> putStrLn ("ERROR: \n" ++ err ++ "\n\nSTATE: \n" ++ show state)
            (Right res, state) -> putStrLn("RESULT: \n" ++ show res ++ "\n\nSTATE: \n" ++ show state)
