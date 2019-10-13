module Chapter6 where

main = putStrLn "Chapter 6"

fac n = product [1..n]

fact_rec 0 = 1
fact_rec n = n*(fact_rec (n + (-1)))

product' [] = 1
product' (n:ns) = n * product' ns

insert_rec x [] = [x]
insert_rec x (y:ys) | x<=y = x : y : ys
                    | otherwise = y : insert_rec x ys


fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2 )

-- from chapter1 alread right?
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    smaller = [xi |xi <-xs, xi<=x]
                    larger = [xi |xi <-xs, xi>x]

-- mutual recursion -> seems kinda cool

even_rec 0 = True
even_rec n = odd_rec (n-1)

odd_rec 0 = False
odd_rec n = even_rec (n-1)

-- Exercises

-- ex1
fact_rec_exc n | n> 0 = n*(fact_rec_exc (n + (-1)))
               | otherwise = 1

-- ex2
sumDown 0 = 0 
sumDown n = n + (sumDown (n-1))

-- ex3: x to the power of n
pow x 0 = 1
pow x n = x * pow x (n-1)

--ex4 euclid
euclid 0 n = n
euclid n 0 = n
euclid n m | n>m = euclid (n-m) m
euclid n m | n<=m = euclid (m-n) n

fibRec ::  (Num a, Ord a) => a -> a
fibRec n    | n < 2 = 1
            | otherwise = fibSerie (n-2) 1 1
    where
        fibSerie n x y  | n == 0 = x
                        | otherwise = fibSerie (n-1) (x+y) x