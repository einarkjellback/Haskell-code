-- This is a collection of functions to solve a Caesar code --
import Data.Char

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

char2int :: Char -> Int
char2int ch = ord ch - ord 'a'

shift :: Char -> Int -> Char
shift ch n  | isLower ch = int2char ((char2int ch + n) `mod` 26)
            | otherwise = ch
            
encode :: String -> Int -> String
encode str n = [shift ch n | ch <- str]

table :: [Float]
table = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Eq a => a -> [a] -> Int
count a xs = sum [1 | x <- xs, a == x]

freqs :: String -> [Float]
freqs str = [percent (count x str) n | x <- ['a'..'z']] where n = length str

chiSq :: [Float] -> [Float] -> Float
chiSq os es = sum [(o - e)^2/e | (o, e) <- zip os es]

rotate :: [a] -> Int -> [a]
rotate xs n = drop n xs ++ take n xs

find :: Eq a => a -> [a] -> [Int]
find e xs = [p | (e',p) <- zip xs [0..], e' == e]

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ x:qsort bigger
    where
        smaller = [a | a <- xs, a <= x]
        bigger = [a | a <- xs, a > x]
        
min' :: Ord a => [a] -> a
min' xs = head (qsort xs)

shiftFac :: String -> Int
shiftFac str = head (find (min' chiValues) chiValues)
    where
        chiValues = [chiSq table (rotate table' i) | i <- [0..25]]
        table' = freqs str

decode :: String -> String
decode str = encode str (-factor)
    where
        factor = shiftFac str