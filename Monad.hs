{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Monad where

-- We use the applicative library
import Control.Applicative

{-

Our main category is the category of haskell types where (called: Hask)
* Obejcts are the concrete types (E.g: Int, Char, (even) Maybe Int, etc, we denote them with variables)
* Isomorphisms are the functions (->)

If 'c' is a parametrized type, it generates a subcategory in our main Hask caterogy (Hask_c)
inside the category of the haskell types. When we fix the c with a concrete
parametrized type we can talk about the subcategory of that given type.

E.g:
'Maybe a' is a subcategory, (Hask_Maybe) where
* Objects are concrete types for the type parameter: Maybe Int, Maybe Char, etc... denoted by 'Maybe a'
* Isomorphism are functions (->) from Maybe a to Maybe b. E.g: (Maybe a) -> (Maybe b)

class Functor c where
  -- The functor class is used for types that can be mapped over.
  fmap :: (a -> b) -> (c a -> c b)

class Functor c => Applicative c where
  pure :: a -> c a
  (<*>) :: c (a -> b) -> c a -> c b

A monad is a functor M:C -> C, along with two morpishms for every object X in C

unit :: X -> M(X)
join :: M(M(X)) -> M(X)

so a monad would be:

class (Functor c) => Monad c where
  unit :: a -> c a
  join :: c (c a) -> c a

in Haskell we use the Kleisli triplet instead of the original notation

class Monad m where
  return :: a -> m a
  (>>=)  :: c a -> (a -> c b) -> c b

Here are some examples with the definition of monad used
in the category theory.
-}

class Applicative c => Monad' c where
  join :: c (c a) => c a

bindKT :: (Monad' c) => c a -> (a -> c b) -> c b
bindKT m k = join ((pure k) <*> m)

instance Monad' c => Monad c where
  return = pure
  (>>=)  = bindKT

-- * Example: Maybe'

data Maybe' a
  = Just' a
  | Nothing'
  deriving (Show)

instance Functor Maybe' where
  fmap f Nothing'  = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure = Just'
  (Just' f) <*> (Just' x) = Just' (f x)
  _         <*> _         = Nothing'

instance Monad' Maybe' where
  join (Just' (Just' x))  = (Just' x)
  join (Just' (Nothing')) = Nothing'
  join Nothing'           = Nothing'

maybe :: Maybe' Int
maybe = do
  x1 <- Just' 3
  x2 <- Nothing'
  return (x1 + x2)

-- * Example: List

newtype List a = List { unList :: [a] }
  deriving (Show)

instance Functor List where
  fmap f (List xs) = List (map f xs)

instance Applicative List where
  pure x = List [x]
  (List fs) <*> (List xs) = List (concatMap (flip map xs) fs)

instance Monad' List where
  join (List lxs) = List (concatMap unList lxs)

list :: List (Int,Int)
list = do
  x <- List [1,2,3]
  y <- List [4,5]
  return (x,y)

{-
With Haskell built-in list would be the following:

instance Functor [] where
  fmap f xs = map f xs

instance Applicative [] where
  pure x = [x]
  fs <*> xs = concat . map (flip map xs) fs

intance Monad' [] where
  join xss = concat xss

-}

-- * Example: Reader

newtype Reader r a = Reader { unReader :: r -> a }

runReader :: r -> Reader r a -> a
runReader x (Reader r) = r x

instance Functor (Reader r) where
  fmap f (Reader r) = Reader (f . r)

instance Applicative (Reader r) where
  pure x = Reader (const x)
  (Reader f) <*> (Reader x) = Reader (\r -> (f r) (x r))

instance Monad' (Reader r) where
  join (Reader fr) = Reader (\r -> unReader (fr r) r)

-- Reader operations
  
readVal :: Reader r r
readVal = Reader id

-- Reader example

reader :: Int
reader = runReader 5 $ do
  x <- return 3
  r <- readVal
  y <- return 2
  return (x + r + y)

-- * Example: State

newtype State s a = State { unState :: s -> (s,a) }

runState :: s -> State s a -> (s,a)
runState x (State s) = s x

instance Functor (State s) where
  fmap f (State fs) = State (\s -> let (s',x) = fs s in (s', f x))

instance Applicative (State s) where
  pure x = State (\s -> (s,x))
  (State f) <*> (State x) = State
    (\s -> let (s',f') = f s
               (s'',x') = x s'
           in (s'', f' x'))

instance Monad' (State s) where
  join (State fs) = State
    (\s -> let (s',(State fs')) = fs s
           in  fs' s')

-- State operations

getState :: State s s
getState = State (\s -> (s,s))

setState :: s -> State s ()
setState s' = State (\_ -> (s',()))

-- State example

state :: (Int,Int)
state = runState 1 $ do
  x <- getState
  setState 2
  y <- getState
  z <- return 3
  return (x + y + z)

