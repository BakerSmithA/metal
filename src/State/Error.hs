module State.Error where

data RuntimeError = UndefVar  -- An undefined function was attempted to be called.
                  | UndefFunc -- An undefined variable was attempted to be used.
