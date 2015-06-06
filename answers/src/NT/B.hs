{-# LANGUAGE DeriveFunctor, RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fdefer-type-errors -fno-warn-unused-imports
                -fno-warn-unused-matches -fno-warn-name-shadowing #-}
-- Copyright (C) 2015  Stephen Compall
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module NT.B where

import Control.Applicative
import Control.Category
import NT.A
import Prelude hiding ((.), id)

{-
What's the forall a. about?

The forall a. means that the function can't talk about the data there.
Consider 'reverse :: forall a. [a] -> [a]': the implementation can't
do anything with the values in the list, not even compare them for
equality, and you certainly can't make up new value of the unknown a
type.  So a function like this alters the "structure" around a,
without touching it.  That is, it transforms the "f" to "g" only, and
is guaranteed to do nothing about the a.

When that 'forall' appears in an argument position, instead of over
the whole value, that means the function gets to choose how to
substitute for that type variable, not you.  So as a user of
'reverse', you get to set 'a' to 'Int' or whatever you like.  But if
you pass 'forall a. [a] -> [a]' as an argument, the user must provide
the choice to the function's implementation to substitute 'a' for
whatever it likes, even types that are indescribable to the caller.

(See Ertugrul Söylemez's 24 DOGE post on rank n types,
https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
, for more comprehensive coverage of this principle.)

Let's talk about what you can do with this, practically.  When you
have a type parameter to some data structure, it's common that you can
"lift" a transformation into it.  So, say you have a data structure
which is like some kind of retrieval interface.
-}

data DS m a b c = DS (a -> m b) (a -> m c)
  deriving (Functor)

{-
There are straightforward ways to transform the a, b, and c
parameters.
-}

mapA :: (x -> a) -> DS m a b c -> DS m x b c
mapA f (DS ab ac) = DS (ab . f) (ac . f)

mapB :: Functor m => (b -> x) -> DS m a b c -> DS m a x c
mapB f (DS ab ac) = DS (fmap f . ab) ac

mapC :: Functor m => (c -> x) -> DS m a b c -> DS m a b x
mapC = fmap -- derived

{-
However, let's say we also want to transform m, using a similar
looking function!  Above we're talking about functions that only
operate on one specific type parameter, lifting a transformation of
that parameter to a transformation of the whole structure.  So there
are a couple of ways we can talk about that transformation.  Here's
one.
-}

mapM1 :: (Applicative f, Functor g)
      => (f (b, c) -> g (b, c))
      -> DS f a b c
      -> DS g a b c
mapM1 f (DS ab ac) = DS (fmap fst . gg) (fmap snd . gg)
  where gg = f . (liftA2 (,) <$> ab <*> ac)

{-
So, the odd thing about this is that it exposes the details of how
mapM1 is implemented: that is, it *must* combine the two original
functions into one and then separate them out.  Of course, then it's
obvious that the function is wrong, because you'll always get both
effects, even if you only want to execute one of the functions.

Let's try again:
-}

mapM2 :: (f b -> g b) -> (f c -> g c) -> DS f a b c -> DS g a b c
mapM2 fb fc (DS ab ac) = DS (fb . ab) (fc . ac)

{-
So, first, we now have to pass our function which is supposedly only
transforming the effect, twice.  Second, we're still revealing details
about the structure of DS that ought to be abstract to the user of a
"lifting" function.  Third, this isn't "pure" transformation of that
m, because each function may know what the underlying 'b' and 'c' are,
and they may be different functions too!

We can fix it by taking an NT as the m-transformer, in one way or
another.
-}

mapM3 :: (forall a. f a -> g a) -> DS f a b c -> DS g a b c
mapM3 f = mapM2 f f

{-
    Exercise B1: Solve this hole using any of the previous functions.
-}

mapM3' :: NT f g -> DS f a b c -> DS g a b c
mapM3' (NT f) = _mapM3'

{-
Now it's impossible for the 'f' argument to do anything but transform
from 'f' to 'g', and that transformation no longer exposes or depends
upon the exact structure of DS.  So, in the same way you lift
functions to transform type parameters of kind *, you lift natural
transformations to transform type parameters of other kinds, like * ->
* in this case.
-}

-- B.hs ends here
