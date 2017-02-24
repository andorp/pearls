{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yoneda
where

-- * Hom representable functor

data Hom a b = Hom (a -> b)

instance Functor (Hom a) where
    fmap g (Hom f) = Hom (g . f)

-- * Natural transformation

newtype f :~> g = Nat (forall a . f a -> g a)

naturality :: (Functor f, Functor g, Eq (g b)) => f :~> g -> (a -> b) -> f a -> Bool
naturality (Nat eta) h a = eta (h <$> a) == (h <$> eta a)

-- * Yoneda lemma:

iso :: Functor f => f a -> (Hom a :~> f)
iso fa = Nat (\(Hom g) -> g <$> fa)

iso' :: Functor f => ((Hom a) :~> f) -> f a
iso' (Nat eta) = eta (Hom id)

newtype Yoneda f a = Yoneda (forall b . (a -> b) -> f b)

-- * CoYoneda lemma:

class Contravariant c where
    cmap :: (a -> b) -> c b -> c a

data CoHom a b = CoHom (b -> a)

instance ContraFunctor (CoHom a) where
    cmap g (CoHom f) = CoHom (f . g)

ciso :: ContraFunctor c => (CoHom a :~> c) -> c a
ciso (Nat eta) = eta (CoHom id)

ciso' :: ContraFunctor c => c a -> (CoHom a :~> c)
ciso' ca = Nat (\(CoHom f) -> cmap f ca)
