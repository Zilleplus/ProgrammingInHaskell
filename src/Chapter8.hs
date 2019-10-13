module Chapter8 where

import Chapter7

main = putStrLn "Chapter8"

-- type declarations

type String' = [Char]

type Pos = (Char,Char)
type Trans = Pos -> Pos

-- Recursive types don't work
-- type Tree = (Int,[Tree])

type Pair' a = (a,a)

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v ) <- t,k == k']

-- data declarations

-- data Bool' = False | True
data Move = North | South | East | West

move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves  [] p = p
moves  (m:ms) p = moves ms (move m p)

rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rect Float Float 

square n =  Rect n n

area (Circle r) = pi * r^2
area (Rect x y) = x*y

-- Newtype declarations

-- newtype Nat = N Int
-- type Nat = Int
-- data Nat = N Int

-- Recursive types:

data Nat = Zero | Succ Nat

nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat 0 = Zero
int2nat n = Succ $ int2nat $ n-1

add Zero n = n
add (Succ m) n = Succ $ add m n

data MyList a = Nil | Cons a (MyList a)
    deriving (Show,Eq)

len Nil = 0
len (Cons _ xs) =  1 + len xs

data MyTree a = Leaf a | Node (MyTree a) a (MyTree a) 
    deriving (Show,Eq)

t = Node (Node (Leaf 1 ) 3 (Leaf 4)) 
         5
         (Node (Leaf 6 ) 7 (Leaf 9)) 

         -- given value occurs in a tree (page 98)

occurs :: Eq a =>a -> MyTree a -> Bool 
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x==y || occurs x l || occurs x r

flatten :: MyTree a -> [a]
flatten (Leaf x) = [x]
flatten (Node left x right) = (flatten left) ++ [x] ++ (flatten right)

-- Class and instance declarations

-- prelude alread defines equality so cant do it here obviously or I would have to hide the prelude definition :-(

-- class MyEq a where 
--     (==), (/=) :: a -> a -> Bool

--     x /=y = not (x==y)

-- instance MyEq MyBool where
--     False == False = True
--     True == True = True
--     _ == _ = False


-- Tautology Checker example ------------------------------------------

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

p1 = And (Var 'A') (Not (Var 'A'))

p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A') 

p3 = Imply (Var 'A') (And (Var 'A') (Var 'B')) 

p4 = Imply (And ( Var 'A') (Imply 
    (Var 'A') (Var 'B'))) (Var 'B')

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const x)  = x
eval sub (Var x)  = find x sub
eval sub (Not p)  = not (eval sub p)
eval sub (And p1 p2)  =  (eval sub p1) && (eval sub p1)
eval sub (Imply p1 p2)  =  (eval sub p1) <= (eval sub p1)

vars :: Prop -> [Char]
vars (Const x) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

-- test 0 _ bits = bits
-- test n bfc bits = test (n-1 ) bfc
--                     ( [odd ((n-1) `div` bfc)] ++ bits)
-- generate all possible combinations of bools
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False: ) bss ++ map (True:) bss
    where 
        bss = bools (n-1)

substs :: Prop -> [Subst]
substs sub = (map zipper) $ bools $ length variables
    where
        variables = rmdups $ vars sub
        zipper vs =  zip variables vs  

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- Tautology Checker example ---------------------------------------END

-- exercise 1

-- data Nat = Zero | Succ Nat
mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ pred_n) m = add (mult pred_n m) m -- n*m=(n-1)*m + m
--  nat2int $ mult (int2nat 2) (int2nat 3)

-- exercise 2

-- data MyTree a = Leaf a | Node (MyTree a) a (MyTree a) 
--     deriving (Show,Eq)
-- t = Node (Node (Leaf 1 ) 3 (Leaf 4)) 
--     5
--     (Node (Leaf 6 ) 7 (Leaf 9)) 
occurs' :: Ord a => a -> MyTree a -> Bool
occurs' e (Node tLeft n tRight) | n == e = True 
                                | n>e = occurs' e tLeft
                                | otherwise = occurs' e tRight
occurs' e (Leaf n) = (n==e) 

-- exercise 3
data NewTree a = NewLeaf a | NewNode (NewTree a) (NewTree a) 
    deriving (Show,Eq)

numberOfLeafs :: NewTree a -> Int
numberOfLeafs (NewLeaf _) = 1
numberOfLeafs (NewNode left right) = numberOfLeafs left + numberOfLeafs right

isBallanced :: NewTree a -> Bool
isBallanced (NewLeaf _) = True
isBallanced (NewNode left right) =  2 < abs ((numberOfLeafs left) - (numberOfLeafs right))
        && isBallanced left 
        && isBallanced right

-- exercise 4
balance :: [a] -> NewTree a
balance l | length l == 1  = NewLeaf ( l!!0 )
          | otherwise  = NewNode (balance (leftSide)) (balance (rightSide))
          where
            half = (length  l) `div` 2
            leftSide = take half  l
            rightSide = drop half l
            
-- exercise 5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) ->(a->a->a) -> Expr -> a
folde f g (Val i) = f i
folde f g (Add i j) = g (folde f g i) (folde f g j)

-- exercise 6
eval_ :: Expr -> Int
eval_  = folde (\x -> x) (+)
