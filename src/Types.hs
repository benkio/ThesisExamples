module Types where

-- SumTypes

data Direction = Left | Right                                    -- Enumerate Type

data Kind      = Club | Heart | Spade | Diamond deriving Eq      -- Enumerate Type

data Tree a    = EmptyTree | Node a (Tree a) (Tree a)            -- Generic Recursive Binary Tree

data List a    = Nil | Cons a (List a)                           -- Generic RecursiveList

-- Product Types

data Pair      = P Int Double                         -- Pair of an Int and a Double

data Card      = C Int Kind                           -- Structure of a Card

-- Type Synonyms

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]


-- Record Type

data Person = Person { firstName :: Name
                     , lastName :: Name
                     , age :: Int
                     }

-- Type Classes

class Equal a where
    same      :: a -> a -> Bool
    different :: a -> a -> Bool

instance Equal Card where
    same      (C x1 k1) (C x2 k2)      = x1 == x2 && k1 == k2
    different (C x1 k1) (C x2 k2)      = x1 /= x2 || k1 /= k2
