module AVL(
    Tree,
    empty,
    isLeaf,
    isElem,
    size,
    insert,
    delete,
    find,
    height,
    inOrder,
    preOrder,
    postOrder,
    foldInOrder,
    foldPreOrder,
    foldPostOrder,
    filterT
) where
import           Data.Maybe (isJust)

data Tree t = Leaf | Node t Int (Tree t) (Tree t) deriving Show

empty :: Tree t
empty = Leaf

isLeaf ::  Tree t -> Bool
isLeaf Leaf = True
isLeaf _     = False

size :: Tree t -> Int
size Leaf            = 0
size (Node _ _ lt rt) = 1 + size lt + size rt

height :: Tree t -> Int
height Leaf          = 0
height (Node _ h _ _) = h

find :: Ord t => t -> Tree t -> Maybe t
find _ Leaf = Nothing
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
rotateLeft _ = Leaf

rotateRight :: Tree t -> Tree t
rotateRight (Node x _ (Node lk _ llt lrt) rt) = newNode lk llt (newNode x lrt rt)
rotateRight _ = Leaf

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
insert x'  Leaf  = newNode x' Leaf Leaf
insert x'  (Node x h lt rt)
  | x'<x          = balance x (insert x' lt) rt
  | x'>x          = balance x lt (insert x' rt)
  | otherwise     = Node x' h lt rt

delete :: (Ord t) => t -> Tree t -> Tree t
delete _ Leaf  = Leaf
delete x' (Node x _ lt rt)
  | x'<x         = balance x (delete x' lt) rt
  | x'>x         = balance x lt (delete x' rt)
  | otherwise    = merge lt rt

merge :: Tree a -> Tree a -> Tree a
merge Leaf rt     = rt
merge lt    Leaf  = lt
merge lt    rt     = balance x' lt rt'
  where (x',rt')  = split rt

split :: Tree t -> (t, Tree t)
split (Node x _ _ rt)  = (x, rt)
split (Node x _ lt    rt)  = (x',balance x lt' rt)
  where (x',lt')  = split lt

inOrder :: Tree t -> [t]
inOrder t  = aux t []
  where
    aux Leaf            xs = xs
    aux (Node x _ lt rt) xs = aux lt (x : aux rt xs)

preOrder :: Tree t -> [t]
preOrder t  = aux t []
  where
    aux Leaf            xs = xs
    aux (Node x _ lt rt) xs = x : aux lt (aux rt xs)

postOrder :: Tree t -> [t]
postOrder t  = aux t []
  where
    aux Leaf            xs = xs
    aux (Node x _ lt rt) xs = aux lt (aux rt (x:xs))

traversal :: ((b -> b) -> (b -> b) -> (b -> b) -> (b -> b)) ->
             (a -> b -> b) -> b -> Tree a -> b
traversal order f z t  = aux t z
  where
    aux Leaf            = id
    aux (Node x _ lt rt) = order (f x) (aux lt) (aux rt)

foldInOrder :: (a -> b -> b) -> b -> Tree a -> b
foldInOrder  = traversal (\xf lf rf -> lf . xf . rf)

foldPreOrder :: (a -> b -> b) -> b -> Tree a -> b
foldPreOrder  = traversal (\xf lf rf -> xf . lf . rf)

foldPostOrder :: (a -> b -> b) -> b -> Tree a -> b
foldPostOrder  = traversal (\xf lf rf -> lf . rf . xf)

instance (Ord a) => Eq (Tree a) where
    (Node x1 _ _ _) == (Node x2 _ _ _) = x1 == x2
    _ == _ = False

instance (Ord a) => Ord (Tree a) where
  (Node x1 _ _ _) `compare` (Node x2 _ _ _) = x1 `compare` x2

instance (Ord a) => Semigroup (Tree a) where
  (<>) = mappend

instance (Ord a) => Monoid (Tree a) where
  mempty = Leaf
  l1 `mappend` l2 = foldl (\x y ->insert y x) l1 l2

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node d _ l r) = foldr f (f d (foldr f z r)) l
  foldl _ z Leaf = z
  foldl f z (Node d _ l r) = foldl f (f (foldl f z l) d) r
  
instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node d c l r) = Node (f d) c (fmap f l ) (fmap f r)

filterT :: (a -> Bool) -> Tree a -> [a]
filterT _ Leaf = []
filterT f (Node d _ l r) | f d = [d] ++ filterT f l ++ filterT f r
                         | otherwise = filterT f l ++ filterT f r