-- https://patternsinfp.wordpress.com/2011/01/31/lenses-are-the-coalgebras-for-the-costate-comonad/
{-# LANGUAGE RankNTypes #-}
module Comonad where

import Prelude hiding ((.), id)
import Control.Category


class Functor w => Comonad w where
    extract   :: w a -> a
    duplicate :: w a -> w (w a)


f <==> g = \x -> f x == g x


type ComonadLaw = forall a w . (Eq (w a), Eq (w (w (w a))), Comonad w)
                => w a -> Bool

law1 :: ComonadLaw
law1 = (extract . duplicate) <==> id

law2 :: ComonadLaw
law2 = (fmap extract . duplicate) <==> id

law3 :: ComonadLaw
law3 = (fmap duplicate . duplicate) <==> (duplicate . duplicate)


data CoState v a = CoState v (v -> a)

instance Functor (CoState v) where
    fmap g (CoState v f) = CoState v (g . f)

instance Comonad (CoState v) where
    extract   (CoState v f) = f v
    duplicate (CoState v f) = CoState v (\u -> CoState u f)


type CoAlgebra c a = a -> c a

type CoAlgebraComonadLaw = forall w a  . (Eq a, Eq (w (w a)), Comonad w)
                         => (a -> w a) -> a -> Bool

law4 :: CoAlgebraComonadLaw
law4 f = (extract . f) <==> id

law5 :: CoAlgebraComonadLaw
law5 f = (duplicate . f) <==> (fmap f . f)

-- Lenses are coalgebras of a costate comonad

{-
QUOTE:
Pierce lenses are pairs of functions between source and target data
type S and T a get function is S -> T and a source function
is a S x T -> S
The story is that the GET is some projection of the data in the source
and so in order to be able to update the source given a modify view, one
needs the original copy of the source to be able to recunstruct the missing
information.
-}

type Get s t = s -> t
type Put s t = s -> (t -> s)
type Lens' s t = (Get s t, Put s t)
type Lens'' s t = s -> (t, t -> s)

type Lens s t = s -> CoState s (t -> s)

data LensW s t = L (Lens s t)

instance Category LensW where
  id = L (\s -> CoState s (const id))
  (L g) . (L f) = L _

