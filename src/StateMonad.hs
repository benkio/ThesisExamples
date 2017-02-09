module StateMonad where

import Control.Monad.Trans.State

-- The signature says that the final value is Unit, but the State computed in S
-- In fact we don't have a value alongside the state.
addOne :: State Int ()
addOne = get >>= \v -> put (v+1)

addOne2 :: State Int ()
addOne2 = do
  v <- get
  put (v + 1)

