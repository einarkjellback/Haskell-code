import Data.List 

data Vertex' val = V' val [Vertex' val]

-- [Vertex a] and [Edge] are sorted
type Digraph a = ([Vertex a], [Edge])

data Vertex val = V Int val
    deriving (Show)
    
instance Eq (Vertex val) where
    V i _ == V j _ = i == j
    
instance Ord (Vertex val) where
    V i _ < V j _ = i < j
    V i _ <= V j _ = i <= j
    V i _ > V j _ = i > j
    V i _ >= V j _ = i >= j

data Edge = E Int Int
    deriving (Show, Eq, Ord)

addV :: Vertex a -> Digraph a -> Digraph a
addV v (vs, es) = (insert v (delete v vs), es)

addE :: Edge -> Digraph a -> Digraph a
addE e (vs, es) = (vs, insert e (delete e es))


-- For every edge in digraph find the vertices it points. Store these in [Vertex' val].
toRecursive :: Digraph a -> Vertex' a
toRecursive [] [] = []
toRecursive [] _ = error "Cannot convert empty Digraph to recursive graph"
toRecursive [(V i val)] es = V' val [(V' j )]
toRecursive ((V i val):vs) es = V' val (map toRecursive childs notChilds)
    where 
        childs = filter comp es
        notChilds = filter (not . comp) es
        comp = \xs -> (getFrom . head xs) == i

neighbors :: Vertex a -> Digraph a -> [Vertex a]
neighbors (V i _) (vs, es) = [v | (E _ j) <- filter (i == (\(E f _) -> f)) es]

inGraph :: Int -> Digraph a -> Bool
inGraph n (vs, _) = elem n vs

vertexIds :: Digraph a -> [Int]
vertexIds (vs, _) = [i | (V i _) <- vs]

getVertex :: Int -> Digraph a -> Vertex a
getVertex n (vs, _) = vs !! n

getFrom, getTo :: Edge -> Int
getFrom (E f t) = f
getTo (E f t) = t