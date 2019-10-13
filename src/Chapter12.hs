module Chapter12 where

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

-- designed my own maybe monad with different name to not collide with prelude
data Maybe' a = None | Some a deriving(Eq,Show)

instance Functor Maybe' where 
    fmap _ None = None
    fmap g (Some x) = Some (g x)

-----------------------------------------------
data Tree a = Leaf a | Node (Tree a) (Tree a)
                deriving Show 

instance Functor Tree where
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l r) = Node (fmap g l) (fmap g r)

-- this doesnt work as fmap is already define as a Functor in the prelude
-- fmap g (Leaf x) = Leaf (g x)
-- fmap g (Node l r) = Node (fmap g l) (fmap g r)

-----------------------------------------------
--instance Applicative Maybe where
--    -- pure :: a -> Maybe a
--    pure = just
--    
--    -- (<*>) :: Maybe (a -> b ) -> Maybe a -> Maybe b
--    Nothing <*> _ = Nothing
--    (Just g) <*> mx = fmap g mx
-----------------------------------------------

-- page 164 Monads
data Expr = Val Int | Dive Expr Expr
