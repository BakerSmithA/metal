module State.EnvSpec (envSpec) where

import State.Env
import Syntax.Tree
import Test.Hspec

-- -- A list of variable definitions which maps variable names to tape symbols.
-- type VarDefs = Map VarName TapeSymbol
--
-- -- A function definitions which maps function names to function bodies.
-- type FuncDefs = Map FuncName Stm
--
-- -- The final environment consists of both a variable and function definitions.
-- data Env = Env {
--     vars  :: VarDefs
--   , funcs :: FuncDefs
-- }
--
-- -- An empty environment containing no variable or function definitions.
-- empty :: Env
-- empty = Env Map.empty Map.empty
--
-- -- Looks up a variable in an environment.
-- lookupVar :: VarName -> Env -> Maybe TapeSymbol
-- lookupVar name env = Map.lookup name (vars env)
--
-- -- Looks up a function in an environment.
-- lookupFunc :: FuncName -> Env -> Maybe Stm
-- lookupFunc name env = Map.lookup name (funcs env)
--
-- -- Adds a single variable to the environment.
-- addVar :: VarName -> TapeSymbol -> Env -> Env
-- addVar name sym env = env { vars = insert name sym (vars env) }
--
-- -- Adds a single function to the environment.
-- addFunc :: FuncName -> Stm -> Env -> Env
-- addFunc name body env = env { funcs = insert name body (funcs env) }

envSpec :: Spec
envSpec = do
    varSpec
    funcSpec

varSpec :: Spec
varSpec = do
    describe "variable environment" $ do
        it "returns Nothing if the variable is undefined" $ do
            lookupVar "x" empty `shouldBe` Nothing

        it "allows variables to be added and retrieved" $ do
            let env = addVar "x" '1' empty
            lookupVar "x" env `shouldBe` Just '1'

        it "overrides previous variable declarations" $ do
            let env  = addVar "x" '1' empty
                env' = addVar "x" '2' env
            lookupVar "x" env' `shouldBe` Just '2'

funcSpec :: Spec
funcSpec = do
    describe "function environment" $ do
        it "returns Nothing if the function is undefined" $ do
            lookupFunc "f" empty `shouldBe` Nothing

        it "allows functions to be added and retrieved" $ do
            let env = addFunc "f" MoveRight empty
            lookupFunc "f" env `shouldBe` Just MoveRight

        it "overrides previous variable declarations" $ do
            let env  = addFunc "f" MoveRight empty
                env' = addFunc "f" MoveLeft env
            lookupFunc "f" env' `shouldBe` Just MoveLeft
