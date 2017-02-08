module IOMonad where

import System.Random

generateAndPrintRandom :: IO ()
generateAndPrintRandom = (randomIO :: IO Int) >>= \r -> print r

generateAndPrintRandom2 :: IO ()
generateAndPrintRandom2 = do
  r <- (randomIO :: IO Int)
  print r
