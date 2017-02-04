module PureFunctionComposition where

pureFunctionComponent1 :: Int -> Int
pureFunctionComponent1 x = x * x

pureFunctionComponent2 :: Int -> Int
pureFunctionComponent2 x = x `mod` 5

pureFunctionCompositionPointStyle :: Int -> Int
pureFunctionCompositionPointStyle = pureFunctionComponent2 . pureFunctionComponent1

pureFunctionComposition :: Int -> Int
pureFunctionComposition x = pureFunctionComponent2(pureFunctionComponent1 x)
