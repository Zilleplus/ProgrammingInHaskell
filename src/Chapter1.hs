module Chapter1 where

    main :: IO()
    main = do
        putStrLn  "Chapter 1"

    
    qsort [] = []
    qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                    where
                        smaller = [xi |xi <-xs, xi<=x]
                        larger = [xi |xi <-xs, xi>x]

    
    -- ex3: the product of []=1 is the internal product, this makes to sense to me so I added and if
    productCostum [] = 0
    productCostum (x:xs) = x * recursiveCall
                        where 
                            recursiveCall = if length xs == 0 then 1 else productCostum xs

    -- ex4
    qsortReverse [] = []
    qsortReverse (x:xs) = qsort larger ++ [x] ++ qsort smaller
                    where
                        smaller = [xi |xi <-xs, xi<=x]
                        larger = [xi |xi <-xs, xi>x]

    -- ex5
    -- the second 2 would belong neither in smaller or larger, and so be removed from the list