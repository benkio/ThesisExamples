module HigherOrderFunction where

algebraApplicator :: (Int -> Int -> Int ) -> Int -> Int -> Int
algebraApplicator f x y = f x y

applySum :: Int -> Int -> Int
applySum x y = let f = algebraApplicator (+)
               in  f x y

applySum' :: Int -> (Int -> Int)
applySum' x = \y -> x + y

ifThenElse :: (a -> b) -> (a -> b) -> Bool -> (a -> b)
ifThenElse f g c = if c then f else g
