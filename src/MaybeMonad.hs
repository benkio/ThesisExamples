module MaybeMonad where

isEven :: Int -> Maybe Int
isEven x = if (x `mod` 2 == 0) then Just x else Nothing

positive :: Int -> Maybe Int
positive x
  | x > 0      = Just x
  | otherwise  = Nothing

subtractionEvenAndPositive :: Int -> Int -> Maybe Int
subtractionEvenAndPositive x y = return (x - y) >>= \x1 -> isEven x1 >>= \x2 -> positive x2

subtractionEvenAndPositive2 :: Int -> Int -> Maybe Int
subtractionEvenAndPositive2 x y = do
  r <- return (x - y)
  r1 <- isEven r
  positive r1
