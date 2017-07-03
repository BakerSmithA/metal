module Semantics.Program where

import Data.List
import Semantics.Helpers
import Semantics.Stm
import State.State
import Syntax.Tree
import System.FilePath.Posix

-- Generates a OS specific filepath from a generic import path.
generatePath :: ImportPath -> FilePath
generatePath = intercalate [pathSeparator]
