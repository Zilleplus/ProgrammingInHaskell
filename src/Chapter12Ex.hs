module Chapter12Ex where
    import Prelude
    import Control.Monad 

    main = putStrLn "Chapter 12 excercises"

    -- 1. Define an instance of the Functor class for the following type of binary trees:
    data Tree a = Leaf | Node (Tree a) a (Tree a)
        deriving Show

    instance Functor Tree where
        fmap _ Leaf = Leaf
        fmap f (Node l a r) = Node lmap fa rmap
            where 
                fa = f a 
                lmap = fmap f l
                rmap = fmap f r
    
    class Functor' f where
        fmap' :: (a -> b) -> f a -> f b

    -- 2. Complete the following instance declaration to make the 
    -- partially-applied function type (a ->) into a functor
    instance Functor' ((->) a) where
        -- fmap' :: (a -> b) ((->)a) ((->)b)
        fmap' = (.)

    -- 3. Define an instance of the Applicative class for the type (a->). 
    -- If you are familiar with combinatory logic,
    -- you might recognise pure and <*> for this type as 
    -- being the well known K and S combinators.
    class Applicative' f where
        pure :: a -> f a
        app :: f (a ->b) -> f a -> f b

    instance Applicative' ((->) a ) where
        pure x = const x -- same as \n -> x
        app g f = \x -> (g x) (f x) -- x is of type b

    -- 4 There may be more than one way to make a prameterised type into an
    -- applicative functor. For example, the library Control.Applicative provides
    -- and aternative zippy instance for lists, in which the function pure makes 
    -- an infirinite list of copies of its argument, and the operator <*> applies each
    -- argument function to the corresponding argument value at the same position.
    -- Complete the following declarations that implement this idea:
    newtype ZipList a = Z [a] deriving Show

    instance Functor ZipList where
        -- fmap :: (a->b) ->  ZipList a -> ZipList b
        fmap f (Z x) = Z (fmap f x)

    instance Applicative ZipList where
        -- pure :: a -> ZipList a
        pure x = Z [x]
        -- <*> :: Z (a -> b) -> Z a -> Z b
        Z fs <*> Z xs = 
            Z (fs <*> xs)

    -- 5. Work out the types for the variables in the four aplicative laws.

    -- 6. Work out the types for the variables in the four apllicative laws
    class Monad' m where
        bind  :: m a -> (a -> m b) -> m b
        return_ ::   a -> m a

    instance Monad' ((->) c) where
        bind n f = \x -> f (n x) x
        return_ a = const a

    -- 7. Given the following type of expression:
    data Expr a  = Var a | Val Int | ADD (Expr a) (Expr a) deriving Show
    
    instance Functor Expr where
        fmap f (Var a) = Var $ f a
        fmap _ (Val i) = Val i
        fmap f (ADD l r) = ADD (fmap f l) (fmap f r)

    instance Applicative Expr where
        -- a -> f a
        pure x = Var x
        -- m (a -> b) -> m a -> m b
        f <*> x = 
