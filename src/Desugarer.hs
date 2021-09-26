module Desugarer (desugar) where

import Monad
import Syntax
import Core

desugar :: [Syntax.Module] -> Astral Core.UntypedModule
desugar = undefined
