module Monad where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as Text

import Front.Syntax

newtype AstralT m a = AstralT (ExceptT String (StateT AstralState m) a)
type Astral = AstralT IO

data AstralState = AstralState
    { _fname :: Maybe FilePath
    , _imports :: [FilePath]
    , _src :: Maybe Text.Text
    , _ast :: Maybe Module
    } deriving (Show)

emptyState :: AstralState
emptyState = AstralState
    { _fname = Nothing
    , _imports = []
    , _src = Nothing
    , _ast = Nothing
    }
