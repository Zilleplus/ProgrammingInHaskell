module Chapter4 where
    
main = putStrLn "Chapter 4"

-- this is new to me and kinda looks cool
-- due to - also beeing used as binary operator call abs'with brackets eg abs'(-2) and not abs' 2
-- abs' ::  (Ord a,Num a) => a -> a
abs' n  | n>=0 = n
        | otherwise = -n

-- just for fun -> assume the list is already sorted, give the element in the list or the element just below
-- blah :: [Int] -> Int -> Int
blah xs n   | length  xs == 1 = if firstElement < n then firstElement else 0
            | length xs == 2 = if last xs <= n then last xs else head xs
            | otherwise = blah [blah firstPart n, blah lastPart n] n
          where 
            firstElement = head xs
            middleIndex = div (length xs) 2
            firstPart = take middleIndex xs
            lastPart = drop middleIndex xs

odds n = map f [0..n-1]
            where f x = x*2 +1

-- exercises

-- ex1 
halve x = (take halfIndex x, drop halfIndex x)
            where halfIndex = div (length x) 2

-- ex2 
-- this makes most sense? (guarded equation was not mentioned in ex2)
third x | length x<3 = Nothing 
        | otherwise = Just ((!!) x 2)

thirdA x = head (tail (tail x))

thirdB x = (!!) x 2

thirdC (_:_:x:_) = x

-- ex3 
safetailA x = if null x then [] else tail x

safetailB x | null x = [] 
           | otherwise = tail x 

safetailC [] = []
safetailC (x:xs) = xs

-- ex4
or1 True True = True
or1 False True = True
or1 True False = True
or1 False False = False

or2 _ True = True
or2 True _ = True
or2 False False = False

or3 False False = False
or3 _ _ = True

or4 False a = a
or4 b False = b
or4 True True = True
