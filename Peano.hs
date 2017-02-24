{-# LANGUAGE FlexibleInstances #-}

import Prelude

data Church' x = Church' ((x -> x) -> x -> x)

-- Sepearate the type parameter in positive and negative occurences
data Church x y = Church ((x -> x) -> x -> y)

runChurch :: Church x y -> (x -> x) -> (x -> y)
runChurch (Church g) f = g f

zero :: Church x x
zero = Church (\_ x -> x)

one :: Church x x
one = Church (\f x -> f x)

suc :: Church x x -> Church x x
suc (Church n) = Church (\f x -> f (n f x))

plus :: Church x x -> Church x x -> Church x x
plus (Church n) (Church m) = Church (\f x -> n f (m f x))

mult :: Church x x -> Church x x -> Church x x
mult (Church n) (Church m) = Church (\f x -> m (n f) x)

instance Num (Church x x) where
    fromInteger 0 = zero
    fromInteger n = suc (fromInteger (n - 1))

    (+) = plus
    (*) = mult

    (-)    = error "(-) is not defined for church numbers"
    abs    = error "abs is not defined for church numners"
    signum = error "signum is not defined for church numbers"

nice :: Church x x
nice = 3

instance Functor (Church x) where
    fmap h (Church g) = Church (\f x -> h (g f x))

instance Applicative (Church x) where
    pure y = Church (\f x -> y)
    (Church g) <*> (Church y) = Church (\f x -> (g f x) (y f x))

joinChurch :: Church x (Church x y) -> Church x y
joinChurch g = Church (\f x -> runChurch ((runChurch g) f x) f x)

instance Monad (Church x) where
    return  = pure
    m >>= k = joinChurch ((pure k) <*> m)

