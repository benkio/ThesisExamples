module ListMonad where

divisors :: Int -> [Int]
divisors n = [x | x <- [1..(n-1)], n `rem` x == 0]

tenMultipliers :: Int -> [Int]
tenMultipliers n = [ f * n | f <- [1..10]]

divisorsMultiplicationTable :: Int -> [Int]
divisorsMultiplicationTable n = divisors n >>= \x -> tenMultipliers x

divisorsMultiplicationTable2 :: Int -> [Int]
divisorsMultiplicationTable2 n = do
  d <- divisors n
  tenMultipliers d

