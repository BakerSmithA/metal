module Semantics.Helpers where

-- Passes the argument `x` to both `f1` and `f2`, the result of `f2` is also 
-- given to `f1`.
both :: (a -> b -> c) -> (b -> a) -> (b -> c)
both f1 f2 x = f1 (f2 x) x
