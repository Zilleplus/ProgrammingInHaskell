module Chapter7 where
import Data.Char
import Data.List

main = putStrLn "Chapter7"

twice f x = f (f x)

foldRight f v [] = v
foldRight f v (x:xs) = f x (foldRight f v xs)

type Bit = Int

bin2int bits = sum [b*w | (b,w) <- zip bits weights]
    where weights = iterate (*2) 1 

-- : is apend ahead (cons)

int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 bits = take 8 (bits ++ repeat 0)

encode = concat . map (make8 . int2bin . ord)

chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode =  map (chr . bin2int) . chop8

-- Voting algo

votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]
count x = length . filter (==x)

rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result vs = sort [(count v vs, v) | v <- rmdups vs]

-- Excercises

--ex1
ex1Start f p xs =  [f x | x <- xs, p x]
-- example : ex1Start (+1) (>4) [2,3,4,5]
ex1 f p = (map f ) . (filter p)
-- example : ex1 (+1) (>4) [2,3,4,5]

--ex2 

-- hmm seems like a/b have a mistake in excercise, should be [a] and not [Bool] 

-- all' p  = (foldl (&&) True) . (map p)
all' p  = and . (map p)

-- any' p  = (foldl (||) False) . (map p)
any' p  = or . (map p)

takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' p [] =  []
dropWhile' p (x:xs) =  if p x then dropWhile' p xs else x:xs

-- ex3 Define using foldr

map' f = foldr (\acc new -> (f acc):new) [] 

filter' p = foldr (\new acc -> 
    if p new then ( [new] ++ acc)
    else acc) [] 
    
-- ex4
dec2int' = ( foldl(\acc (p, n) -> p*n + acc) 0 ) . (zip powers) . reverse -- own solution is somewhat complicated
    where
        powers = iterate (*10) 1

-- solution from book:
dec2int''  :: Num a => [a] -> a -- type system cant handle it without this
dec2int''  = foldl (\x y -> x*10 +y ) 0 

