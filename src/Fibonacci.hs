module Fibonacci where

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)

fibTailRecursive :: Int -> Int
fibTailRecursive x = fibHelp 0 1 x
                     where
                       fibHelp a b n = if n > 0 then fibHelp b (a+b) (n-1) else a
