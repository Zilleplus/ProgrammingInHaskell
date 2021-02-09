module Chapter12 where
    import Prelude
    import Control.Monad

    main = putStrLn "Chapter 12"

    -- Functors
    inc :: [Int] -> [Int]
    inc [] = []
    inc (n:ns) = n+1 : inc ns

    sqr :: [Int] -> [Int]
    sqr [] = []
    sqr (n:ns) = n^2 : sqr ns

    map' :: (a -> b) -> [a] -> [b]
    map' f [] = []
    map' f (x:xs) = f x : map' f xs

    inc' = map' (+1)
    sqr'= map' (^2)

    class Functor' f where
        fmap' :: (a->b) -> f a -> f b

    instance Functor' [] where
        fmap' = map'

    instance Functor' IO where
        -- fmap' :: (a->b) -> IO a -> IO b
        fmap' g mx = do {x <- mx; return (g x)}

    -- Define my own maybe monad with different name to not collide with prelude
    data Maybe' a = None | Some a deriving(Eq,Show)

    instance Functor Maybe' where
        fmap _ None = None
        fmap g (Some x) = Some (g x)

    --------------------------------------------
    data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

    instance Functor Tree where
        fmap g (Leaf x) = Leaf (g x)
        fmap g (Node l r) = Node (fmap g l) (fmap g r)

    -----------------------------------------------
    instance Applicative Maybe' where
        --pure :: a -> Maybe' a
        pure = Some
        
        -- (<*>):: f (a -> b) -> f a -> f b
        None <*> _  = None
        (Some g) <*> mx = fmap g mx

    -- instance Applicative IO where
    --     pure = return

    --     mg <*> mx = do {g <- mg; x <- mx; return (g x)}
    
    -----------------------------------------------


    --demo1 = pure (+1) <*> [1,2,3]
    --demo1b = (+1) <$> [1,2,3]

    --demo2 = (+) <$> [1] <*> [2]

    --  prods [1,2] [3,4] results in  [3,4,6,8]
    --prods :: [Int] -> [Int] -> [Int]
    --prods xs ys = [x*y | x <-xs , y <- ys]

    -- alternative impl of prods using applicatives
    --prodsApplicative :: [Int] -> [Int] -> [Int]
    --prodsApplicative x y = (*) <$> x <*> y
    --
    -- page 164 Monads
    data Expr = Val Int | Div Expr Expr

    eval :: Expr -> Int
    eval (Val n) = n
    eval (Div x y ) = eval x `div` eval y

    safediv :: Int -> Int -> Maybe Int
    safediv _ 0 = Nothing
    safediv x y = Just $ x `div` y

    -- Doesn't work cuz safediv should have (int->int->int) to
    -- work with applicatives.
    -- eval2 :: Expr -> Maybe Int
    -- eval2 (Val n) = pure n
    -- eval2 (Div x y) = safediv <$> eval2 x <*> eval2 y


    -- This is already defined in prelude
    -- (>>=) :: Maybe a -> (a-> Maybe b) -> Maybe b
    -- mx (>>=) f = case mx of 
    --              Nothing -> Nothing
    --              Just x -> f x

    eval3 :: Expr -> Maybe Int
    eval3 (Val n) = Just n
    eval3 (Div x y) = (eval3 x) >>= \n ->
        (eval3 y) >>= \m ->
        safediv n m
