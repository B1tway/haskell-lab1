module AVL(
    Tree,
    empty,
    isEmpty,
    isElem,
    size,
    insert,
    find,
    height
) where
import           Data.Maybe (isJust)

data Tree t = Empty | Node t Int (Tree t) (Tree t) deriving Show

empty :: Tree t
empty = Empty

isEmpty ::  Tree t -> Bool
isEmpty Empty = True
isEmpty _     = False

size :: Tree t -> Int
size Empty            = 0
size (Node _ _ lt rt) = 1 + size lt + size rt

height :: Tree t -> Int
height Empty          = 0
height (Node _ h _ _) = h

find :: Ord t => t -> Tree t -> Maybe t
find _ Empty = Nothing
find x' (Node x _ lt rt)
    | x' < x = find x' lt
    | x' > x = find x' rt
    | otherwise = Just x

isElem :: Ord t => t -> Tree t -> Bool
isElem x t  = isJust (find x t)


newNode :: t -> Tree t -> Tree t -> Tree t
newNode x lt rt  = Node x h lt rt
 where h  = 1 + max (height lt) (height rt)


rotateLeft :: Tree t -> Tree t
rotateLeft (Node x _ lt (Node rk _ rlt rrt)) = newNode rk (newNode x lt rlt) rrt
rotateLeft _ = Empty

rotateRight :: Tree t -> Tree t
rotateRight (Node x _ (Node lk _ llt lrt) rt) = newNode lk llt (newNode x lrt rt)
rotateRight _ = Empty

rightLeaning :: Tree t -> Bool
rightLeaning (Node _ _ lt rt) = height lt < height rt
rightLeaning _                = False

leftLeaning :: Tree t -> Bool
leftLeaning (Node _ _ lt rt) = height lt > height rt
leftLeaning _                = False

balance :: t -> Tree t -> Tree t -> Tree t
balance k lt rt
  | lh-rh > 1 && leftLeaning lt   = rotateRight (newNode k lt rt)
  | lh-rh > 1                     = rotateRight (newNode k (rotateLeft lt) rt)
  | rh-lh > 1 && rightLeaning rt  = rotateLeft (newNode k lt rt)
  | rh-lh > 1                     = rotateLeft (newNode k lt (rotateLeft rt))
  | otherwise                       = newNode k lt rt
  where lh  = height lt
        rh  = height rt

insert :: (Ord a) => a -> Tree a -> Tree a
insert x'  Empty  = newNode x' Empty Empty
insert x'  (Node x h lt rt)
  | x'<x          = balance x (insert x' lt) rt
  | x'>x          = balance x lt (insert x' rt)
  | otherwise     = Node x' h lt rt
