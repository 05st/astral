{-# Language GeneralizedNewtypeDeriving #-}

module Monad where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as Text
import qualified Data.Map as Map

import Base.Name
import Base.Type
import Syntax

type Error = String

newtype AstralT m a = AstralT { runAstralT :: ExceptT Error (StateT AstralState m) a }
    deriving (Functor, Applicative, Monad, MonadError Error)
type Astral = AstralT IO

data AstralState = AstralState
    { _fname :: Maybe FilePath
    , _imports :: [FilePath]
    , _src :: Maybe Text.Text
    , _ast :: Maybe Module
    , _tenv :: Map.Map Name Type
    , _kenv :: Map.Map Name Kind
    } deriving (Show)

emptyState :: AstralState
emptyState = AstralState
    { _fname = Nothing
    , _imports = []
    , _src = Nothing
    , _ast = Nothing
    , _tenv = Map.empty
    , _kenv = Map.empty
    }

runAstral :: AstralT m a -> AstralState -> m (Either Error a, AstralState)
runAstral = runStateT . runExceptT . runAstralT
