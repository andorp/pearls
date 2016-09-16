{-# LANGUAGE EmptyCase     #-}
{-# LANGUAGE TypeOperators #-}
module Arithmetic where

import Prelude hiding (Either(..))

{-
Simple type arithmetic
http://category-theory.mitpress.mit.edu/chapter003.html
Proposition 3.4.3.1

0 = Empty type
1 = Unit type

1)  A + 0          ~  A
2)  A + B          ~  B + A
3)  (A + B) + C    ~  A + (B + C)
4)  A * 0          ~  0
5)  A * 1          ~  A
6)  A * B          ~  B * A
7)  A * (B * C)    ~  (A * B) * C
8)  A * (B + C)    ~  A * B + A * C
9)  0 -> A         ~  1
10) 1 -> A         ~  A
11) A -> 0         ~  0
12) A -> 1         ~  1
13) (B + C) -> A   ~  (B -> A) * (C -> A)
14) C -> (B -> A)  ~  (C * B) -> A
15) C -> (A * B)   ~  (C -> A) * (C -> B)

-}

data Zero
data One = One

data (+) l r = Left l | Right r
data (*) l r = Pair l r

-- 1) A + 0 ~  A

one_lr :: a + Zero -> a
one_lr (Left x) = x
one_lr (Right x) = case x of

one_rl :: a -> a + Zero
one_rl x = Left x

-- 2)  A + B  ~  B + A

two_lr :: a + b -> b + a
two_lr (Left a) = Right a
two_lr (Right b) = Left b

two_rl :: b + a -> a + b
two_rl (Left b)  = Right b
two_rl (Right a) = Left a

-- 3)  (A + B) + C    ~  A + (B + C)

three_lr :: ((a + b) + c) -> (a + (b + c))
three_lr (Left (Left a))  = Left a
three_lr (Left (Right b)) = Right (Left b)
three_lr (Right c)        = Right (Right c)

three_rl :: (a + (b + c)) -> ((a + b) + c)
three_rl (Left a)          = Left (Left a)
three_rl (Right (Left b))  = Left (Right b)
three_rl (Right (Right c)) = Right c

-- 4)  A * 0  ~  0

four_lr :: a * Zero -> Zero
four_lr (Pair l r) = case r of

four_rl :: Zero -> a * Zero
four_rl x = case x of

-- 5)  A * 1  ~  A

five_lr :: a * One -> a
five_lr (Pair x One) = x

five_rl :: a -> a * One
five_rl x = Pair x One

-- 6)  A * B  ~  B * A

six_lr :: a * b -> b * a
six_lr (Pair a b) = Pair b a

six_rl :: b * a -> a * b
six_rl (Pair b a) = Pair a b

-- 7)  A * (B * C)    ~  (A * B) * C

seven_lr :: (a * (b * c)) -> ((a * b) * c)
seven_lr (Pair a (Pair b c)) = (Pair (Pair a b) c)

seven_rl :: ((a * b) * c) -> (a * (b * c))
seven_rl (Pair (Pair a b) c) = Pair a (Pair b c)

-- 8)  A * (B + C)    ~  A * B + A * C

eight_lr :: a * (b + c) -> (a * b) + (a * c)
eight_lr (Pair a (Left  b)) = Left  (Pair a b)
eight_lr (Pair a (Right c)) = Right (Pair a c)

eight_rl :: (a * b) + (a * c) -> a * (b + c)
eight_rl (Left  (Pair a b)) = Pair a (Left b)
eight_rl (Right (Pair a c)) = Pair a (Right c)

-- 9)  0 -> A         ~  1

nine_lr :: (Zero -> a) -> One
nine_lr f = One

nine_rl :: One -> (Zero -> a)
nine_rl One = \zero -> case zero of

-- 10) 1 -> A         ~  A

ten_lr :: (One -> a) -> a
ten_lr f = f One

ten_rl :: a -> (One -> a)
ten_rl a = \one -> case one of One -> a

-- 11) A -> 0         ~  0

eleven_lr :: (a -> Zero) -> Zero
eleven_lr f = case f undefined of

eleven_rl :: Zero -> (a -> Zero)
eleven_rl z = \a -> case a of

-- 12) A -> 1         ~  1

twelve_lr :: (a -> One) -> One
twelve_lr _f = One

twelve_rl :: One -> (a -> One)
twelve_rl One = \a -> One

-- 13) (B + C) -> A   ~  (B -> A) * (C -> A)

thirteen_lr :: ((b + c) -> a) -> ((b -> a) * (c -> a))
thirteen_lr bca = Pair (\b -> bca (Left b)) (\c -> bca (Right c))


thirteen_rl :: ((b -> a) * (c -> a)) -> ((b + c) -> a)
thirteen_rl (Pair l r) = \bc -> case bc of
    Left  b -> l b
    Right c -> r c

-- 14) C -> (B -> A)  ~  (C * B) -> A

fourteen_lr :: (c -> (b -> a)) -> ((c * b) -> a)
fourteen_lr f = \p -> case p of
    Pair l r -> f l r

fourteen_rl :: ((c * b) -> a) -> (c -> (b -> a))
fourteen_rl f = \c -> \b -> f (Pair c b)

-- 15) C -> (A * B)   ~  (C -> A) * (C -> B)

fifthteen_lr :: (c -> (a * b)) -> ((c -> a) * (c -> b))
fifthteen_lr f =
    Pair (\c -> case f c of Pair l _ -> l)
         (\c -> case f c of Pair _ r -> r)

fifthteen_rl :: ((c -> a) * (c -> b)) -> (c -> (a * b))
fifthteen_rl (Pair l r) = \c -> Pair (l c) (r c)
