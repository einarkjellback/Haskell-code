data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ Zero) n = n
mult m (Succ Zero) = m
mult (Succ m) n = add n (mult m n)

{-
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = compare x y == EQ
occurs x (Node l y r) 
    | c == EQ = True
    | c == GT = occurs x r
    | otherwise = occurs x l
    where
        c = compare x y
-}
        
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

countLeafs :: Tree a -> Int
countLeafs (Leaf _) = 1
countLeafs (Node l r) = countLeafs l + countLeafs r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs leafDiff <= 1
    where
        leafDiff = countLeafs l - countLeafs r
        
split :: [a] -> ([a],[a])
split xs = (take half xs, drop half xs)
    where
        half = length xs `div` 2
        
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance l) (balance r)
    where 
        l = fst (split xs)
        r = snd (split xs)
        
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val a) = f a
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\_ -> 1) (+)

data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
    Nothing' == Nothing' = True
    Nothing' == Just' _ = False
    Just' a == Just' b = a == b
    
instance Eq a => Eq [a] where
    [] == [] = True
    [] == _ = False
    _ == [] = False
    (x:xs) == (y:ys) = x == y && xs == ys