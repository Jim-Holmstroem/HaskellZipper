{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding (Left, Right)
import Control.Monad (liftM3, liftM4)
import Test.QuickCheck

data Tree a = Empty | Node (Tree a) a (Tree a)
    deriving (Show, Eq)

data Direction = Left | Right
    deriving (Show, Eq)

instance Arbitrary Direction where
    arbitrary = elements [Left, Right]

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [return Empty, liftM3 Node arbitrary arbitrary arbitrary]
    shrink Empty = []
    shrink (Node left value right) =
        [Empty] ++
        [left, right] ++
        [Node left' value' right' | (left', value', right') <- shrink (left, value, right)]

instance (Arbitrary a) => Arbitrary (TreeZipper a) where
    arbitrary = liftM4 TreeZipper arbitrary arbitrary arbitrary arbitrary


data NamedOperation a = NamedOperation String (TreeZipper a -> TreeZipper a)

upOperation, leftOperation, rightOperation, idOperation :: NamedOperation a
upOperation = NamedOperation "up" up
leftOperation = NamedOperation "left" left
rightOperation = NamedOperation "right" right
idOperation = NamedOperation "id" id

instance Show (NamedOperation a) where
    show (NamedOperation name _) = show name
(NamedOperation fName f) .> (NamedOperation gName g) = NamedOperation (fName ++ "." ++ gName) (f . g)
(NamedOperation _ f) $> x = f x
instance Arbitrary (NamedOperation a) where
    arbitrary = elements [upOperation, leftOperation, rightOperation]


-- misses instance Show so it cannot directly be used in quickCheck as argument, use NamedOperation
instance Arbitrary (TreeZipper a -> TreeZipper a) where
    arbitrary = elements [up, left, right]

singleton :: a -> Tree a
singleton x = Node Empty x Empty
levels :: [a] -> Tree a
levels (x:xs) = Node (levels xs) x (levels xs)
levels [] = Empty

l3 = levels [1..3]
l3z = treeZipper l3

-- dT = T^2 + L(2xT)
-- TreeZipper [(directionOfAlternativeBranch, rootValueOfAlternativeBranch, alternativeBranch)] focus left right
data TreeZipper a = TreeZipper [(Direction, a, Tree a)] (Maybe a) (Tree a) (Tree a)
    deriving (Show, Eq)


treeZipper :: Tree a -> TreeZipper a
treeZipper (Node left focus right) = TreeZipper [] (Just focus) left right
treeZipper Empty = TreeZipper [] Nothing Empty Empty

toTree :: TreeZipper a -> Tree a
toTree (TreeZipper [] (Just focus) left right) = Node left focus right
toTree (TreeZipper [] Nothing _ _) = Empty
toTree tz@(TreeZipper upv@(_:_) _ _ _) = toTree $ up tz

focus :: TreeZipper a -> Maybe a
focus (TreeZipper _ x _ _) = x

-- the up,left,right will have no effect (it will stay the same) if it's already at the respective edge (i.e. it has no where to go in that direction). This is just for simplicity
up,left,right :: TreeZipper a -> TreeZipper a
left (TreeZipper upv (Just focus) left@(Node leftLeft leftValue leftRight) right) = TreeZipper upv' focus' left' right'
    where upv' = (Right, focus, right):upv
          focus' = Just leftValue
          left' = leftLeft
          right' = leftRight
left tz@(TreeZipper _ Nothing _ _) = tz
left tz@(TreeZipper _ _ Empty _) = tz
right (TreeZipper upv (Just focus) left right@(Node rightLeft rightValue rightRight)) = TreeZipper upv' focus' left' right'
    where upv' = (Left, focus, left):upv
          focus' = Just rightValue
          left' = rightLeft
          right' = rightRight
right tz@(TreeZipper _ Nothing _ _) = tz
right tz@(TreeZipper _ _ _ Empty) = tz
up (TreeZipper (up@(Left, upValue, upTree):ups) (Just focus) left right) = TreeZipper upv' focus' left' right'
    where upv' = ups
          focus' = Just upValue
          left' = upTree
          right' = Node left focus right
up (TreeZipper (up@(Right, upValue, upTree):ups) (Just focus) left right) = TreeZipper upv' focus' left' right'
    where upv' = ups
          focus' = Just upValue
          left' = Node left focus right
          right' = upTree
up tz@(TreeZipper [] _ _ _) = tz
up tz@(TreeZipper _ Nothing _ _) = tz


-- TODO needs more tests

prop_upOnRoot :: Tree Int -> Bool
prop_upOnRoot tree = zippedTree == up zippedTree
    where zippedTree = treeZipper tree

prop_operationEquivalence :: Tree Int -> [NamedOperation Int] -> Bool
prop_operationEquivalence tree operations = tree' == tree
    where tree' = toTree $  (foldl (.>) idOperation operations) $> (treeZipper tree)
