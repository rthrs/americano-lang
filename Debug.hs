module Debug where

import Control.Monad

import Americano.Printer
import Americano.Semantics.Domains

__DEBUG__ = False
__SHOW_ABSTRACT_SYNTAX__ = False

runDebug :: (Monad m) => m () -> m ()
runDebug f = when __DEBUG__ f 

printTree :: (Show a, Print a) => a -> IO ()
printTree tree = when __SHOW_ABSTRACT_SYNTAX__ $ putStrLn $ showTree tree

showTree :: (Show a, Print a) => a -> String
showTree tree = "[Abstract Syntax] " ++ show tree

printResult :: Env -> Store -> IO ()
printResult env store = putStrLn $ (showEnv env) ++ (showStore store)

showEnv :: Env -> String
showEnv env = "[Environment] " ++ show env

showStore :: Store -> String
showStore store = "[Store] " ++ show store
