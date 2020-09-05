import Data.Char

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

bin2int :: [Bit] -> Int
bin2int b = sum [c*order | (c, order) <- zip b series]
    where
        series = iterate (*2) 1
        
normalTo8 :: [Bit] -> [Bit]
normalTo8 n = take 8 (n ++ [0,0..])

char2bin :: Char -> [Bit]
char2bin = normalTo8 . int2bin . ord

str2bin :: String -> [Bit]
str2bin = concat . map (normalTo8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bs = take 9 bs : chop9 (drop 9 bs)

bin2str :: [Bit] -> String
bin2str = map (chr . bin2int) . chop8

parityProof :: [Bit] -> [Bit]
parityProof bs = concat (map (\xs -> xs ++ [parity xs]) (chop8 bs))

parity :: [Bit] -> Int
parity bs = mod (length (filter (==1) bs)) 2

errorFree :: [Bit] -> Bool
errorFree bs = if all (\xs -> last xs == parity (init xs)) (chop9 bs) then True else error "Faulty string"

transmit :: String -> String
transmit = bin2str . str2bin

