{-# LANGUAGE RankNTypes, KindSignatures, GADTs, PolyKinds, TypeFamilies, LambdaCase #-}
module Adjunction where

import Prelude hiding (Functor(..), product)
import Data.Monoid (Sum(..))

-- * Categories

data Category (c :: * -> * -> *) = Category
  { src :: forall x y   . c x y -> c x x
  , tgt :: forall x y   . c x y -> c y y
  , cmp :: forall x y z . c y z -> c x y -> c x z
  }



type Hask = (->)
hask :: Category Hask
hask = Category
  { src = \_ -> \x -> x
  , tgt = \_ -> \x -> x
  , cmp = \f g -> f . g
  }



newtype SubHask f a b = SH (f a -> f b)
subCategory :: Category (SubHask f)
subCategory = Category
  { src = \_ -> SH id
  , tgt = \_ -> SH id
  , cmp = \(SH g) (SH f) -> SH (g . f)
  }



data Op (c :: * -> * -> *) a b = Op (c b a)

op :: Category c -> Category (Op c)
op c1 = Category
  { src = \(Op a) -> Op (tgt c1 a)
  , tgt = \(Op a) -> Op (src c1 a)
  , cmp = \(Op a) (Op b) -> Op (cmp c1 b a)
  }



data Product (c1 :: * -> * -> *) (c2 :: * -> * -> *) a b where
  Product :: (c1 a1 b1) -> (c2 a2 b2) -> Product c1 c2 (a1,a2) (b1,b2)

product :: Category c1 -> Category c2 -> Category (Product c1 c2)
product c1 c2 = Category
  { src = \(Product a1 a2) -> Product (src c1 a1) (src c2 a2)
  , tgt = \(Product a1 a2) -> Product (tgt c1 a1) (tgt c2 a2)
  , cmp = \(Product a1 b1) (Product a2 b2) -> Product (cmp c1 a1 a2) (cmp c2 b1 b2)
  }

-- Functor

type family FunctorT f t = r
data Functor f c1 c2 = Functor
  { fmap
      :: forall x1 y1 x2 y2
       . (FunctorT f x1 ~ x2, FunctorT f y1 ~ y2)
      => c1 x1 y1 -> c2 x2 y2
  }

data MaybeF
type instance FunctorT MaybeF a = a
maybeF2 :: Functor MaybeF Hask (SubHask Maybe)
maybeF2 = Functor
  { fmap = \f -> SH $ \case
      Nothing -> Nothing
      Just x  -> Just $ f x
  }


data DiagF
type instance FunctorT DiagF a = (a,a)
diagF :: Functor DiagF Hask (Product Hask Hask)
diagF = Functor { fmap = \f -> Product f f }



data CSF f
type instance FunctorT (CSF f) a = f a
csF :: Functor (CSF f) (SubHask f) Hask
csF = Functor { fmap = \(SH f) -> f }


-- * Adjunction

data Adjunction c1 c2 l r = Adjunction
  { leftA  :: forall a b la rb . (FunctorT l a ~ la, FunctorT r b ~ rb) => c1 la b -> c2 a rb
  , rightA :: forall a b la rb . (FunctorT l a ~ la, FunctorT r b ~ rb) => c2 a rb -> c1 la b
  }

data Family (a :: *) = Family

data PairT a
data Pair a b = Pair a b
type instance FunctorT (PairT a) b = Pair a b
paF2 :: Family a -> Functor (PairT a) Hask Hask
paF2 _ = Functor { fmap = \f (Pair x a) -> Pair x (f a) }


data FunT a
data Fun a b = Fun (a -> b)
type instance FunctorT (FunT a) b = Fun a b
faF :: Family a -> Functor (FunT a) Hask Hask
faF _ = Functor { fmap = \f (Fun g) -> Fun (f . g) }

spice :: Family a -> Adjunction Hask Hask (PairT a) (FunT a)
spice _ = Adjunction
  { leftA  = \f a          -> Fun $ \x -> f (Pair x a)
  , rightA = \f (Pair x a) -> case f a of { Fun g -> g x }
  }

-- * Two category

data Fls
data Tru

data Boolean a b where
  Fls :: Boolean Fls Fls
  F2T :: Boolean Fls Tru
  Tru :: Boolean Tru Tru

srcB :: forall a b . Boolean a b -> Boolean a a
srcB Fls = Fls
srcB F2T = Fls
srcB Tru = Tru

tgtB :: forall a b . Boolean a b -> Boolean b b
tgtB Fls = Fls
tgtB F2T = Tru
tgtB Tru = Tru

cmpB :: forall a b c . Boolean b c -> Boolean a b -> Boolean a c
cmpB Fls Fls = Fls
cmpB F2T Fls = F2T
cmpB Tru F2T = F2T
cmpB Tru Tru = Tru

booleanCat :: Category Boolean
booleanCat = Category
  { src = srcB
  , tgt = tgtB
  , cmp = cmpB
  }

-- * Category of a monoid

data O
data Mon m a b where
  Mon :: Monoid m => m -> Mon m O O

monSrc :: forall m x y . Mon m x y -> Mon m x x
monSrc (Mon _) = Mon mempty

monTgt :: forall m x y . Mon m x y -> Mon m y y
monTgt (Mon _) = Mon mempty

monCmp :: forall m a b c . Mon m b c -> Mon m a b -> Mon m a c
monCmp (Mon m2) (Mon m1) = Mon (m1 <> m2)

mon :: Category (Mon m)
mon = Category
  { src = monSrc
  , tgt = monTgt
  , cmp = monCmp
  }

data MT
type instance FunctorT MT o = o
monFunctor :: Functor MT (Mon [a]) (Mon (Sum Int))
monFunctor = Functor { fmap = \(Mon as) -> Mon $ Sum $ length as }


-- * Category of a PreOrder

data X
data PreOrder p a b where
  Emp :: PreOrder p X X
  Ext :: (p,p) -> PreOrder p X X

preSrc :: forall p x y . (Eq p) => PreOrder p x y -> PreOrder p x x
preSrc Emp         = Emp
preSrc (Ext (s,t)) = Ext (s,s)

preTgt :: forall p x y . (Eq p) => PreOrder p x y -> PreOrder p y y
preTgt Emp         = Emp
preTgt (Ext (s,t)) = Ext (t,t)

preCmp :: forall p a b c . (Eq p) => PreOrder p b c -> PreOrder p a b -> PreOrder p a c
preCmp Emp   Ext{} = Emp
preCmp Ext{} Emp   = Emp
preCmp (Ext (s1,t1)) (Ext (s2,t2))
  | t1 == s2  = Ext (s1, t2)
  | otherwise = Emp

preOrderCat :: (Eq p) => Category (PreOrder p)
preOrderCat = Category
  { src = preSrc
  , tgt = preTgt
  , cmp = preCmp
  }

{-
data PF
type instance FunctorT PF b = b
paF :: Functor PF Hask (SubHask (P x))
paF = Functor { fmap = \f -> SH $ \(P x a) -> P x (f a) }

data FF
data F a b = F (a -> b)
type instance FunctorT FF b = b
faF :: Functor FF Hask (SubHask (F x))
faF = Functor { fmap = \f -> SH $ \(F g) -> F (f . g) }
-}

-}
