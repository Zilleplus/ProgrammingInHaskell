module Chapter5 where

import Data.Char

main = putStrLn "Chapter5"

let2int c = ord c - ord 'a'
int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c  

encode n xs = [shift n x | x<-xs]

-- ex1
ex1 = sum [x^2 | x <- [1..100]]

-- ex2
grid n m = [(x,y) | x<-[0..n], y<-[0..m]]

-- ex3
square n = [(x,y) | x<-[0..n], y<-[0..n], x/=y]

-- ex4 replicate x n time
replicate' n x = [x | _ <-[1..n]]

-- ex5 pythagorean
pythos n = [(x,y,z)|x<-[1..n], y<-[1..n],z<-[1..n], x^2 + y^2 == z^2 ]

-- ex6 perfect
factors n = [ x | x <- [1..n-1], mod n x == 0]
perfects n = [x | x <- [1..n-1], sum (factors x) == x ]
