module State.ProgStateSpec (stateSpec) where

import State.Env
import State.Machine
import State.ProgState
import Test.Hspec

-- newtype ProgState a = ProgState { runState :: ReaderT Env Machine a }
--
-- instance Functor ProgState where
--     -- fmap :: (a -> b) -> (ProgState a) -> (ProgState b)
--     fmap f = ProgState . fmap f . runState

stateSpec :: Spec
stateSpec = undefined

-- stateSpec = do
--     describe "State" $ do
--         fmapSpec
--         appSpec
--         bindSpec
--
-- fmapSpec :: Spec
-- fmapSpec = do
--     describe "fmap" $ do
--         it "maps the state" $ do
--             let s  = ProgState (return 3)
--                 s' = fmap (+1) s
--
--             runProgState s' initial `shouldBe` (return 4)
--
-- appSpec :: Spec
-- appSpec = do
--     describe "app" $ do
--         it "applies the function" $ do
--             let f  = ProgState (return (+1))
--                 s  = ProgState (return 3)
--                 s' = f <*> s
--
--             runProgState s' initial `shouldBe` (return 4)
--
-- bindSpec :: Spec
-- bindSpec = do
--     describe "bind" $ do
--         it "binds" $ do
--             let s  = ProgState (return 3)
--                 f  = return . (+1)
--                 s' = s >>= f
--
--             runProgState s' initial `shouldBe` (return 4)
