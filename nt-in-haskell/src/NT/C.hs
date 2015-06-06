{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures
                -fno-warn-unused-imports -fno-warn-unused-matches #-}

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
module NT.C where

import Control.Applicative (Const(..))
import Control.Monad
import Control.Monad.State
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Traversable (sequenceA)
import NT.A

{-
What other functions are like this?

'join', for one.  Join has the form:

join :: forall a. m (m a) -> m a

To make this fit in 'NT', we need to find values for 'f' and 'g'.

    Exercise C1: What happens when you try to evaluate 'NT join'?

Here is the trouble.  When you are implementing a function with one
position that's universally quantified, as 'a' is in 'forall a. f a ->
g a', 'a' is existential within the definition.  That is, it is
guaranteed to be some type, and if you have two values of that type,
they have the same type, but you don't know what that type is.

When you try to apply 'NT' to 'join', it uses one definite shape of
the type.  That is, when you try to unify 'f a' with 'm (m b)'
(variable renamed for clarity), there is only one allowed
interpretation of the latter type in terms of the former: 'f = m' and
'a = m b'.  But 'a' is existential!  You're trying to let information
escape about a universally quantified type variable, by saying that it
has a type constructor equal to the known type constructor that
surrounds the 'm a'.

What if I try to do some trickery with type synonyms?  Let's give it a
shot.
-}

type Doubled m a = m (m a)

{-
NT (join :: Monad m => Doubled m a -> m a)

You get the same error, instead of a unification where 'f = Doubled m'
and 'g = m'.

The problem is that type synonyms are not real types.  For any given
type, there is exactly one way to destructure it by separating type
constructors from arguments.  The formula is as follows:

1. Expand all type synonyms.  If any type synonyms are partially
   applied, it is an invalid type.

2. If the resulting type is of the form (...) (...), it is a type
   application.  Otherwise, the type cannot be separated into 'f' and
   'a'.

3. Optionally, substitute type synonyms back in.

This is why 'Except' is defined as

type Except e = ExceptT e Identity

instead of

type Except e a = ExceptT e Identity a

because for the former, a single argument is enough to make the
synonym fully applied; it is more useful in every way than the second
definition.

    Exercise C2: Separate these types into 'f' and 'a', or say that it
    is impossible.

String
State Int Int
Identity Int
String -> [String]
Const [Int] ()

Now, this is a good thing, because if they were real types, you'd be
able to write nonsense like this:
-}

-- Try uncommenting this!

-- instance Functor f => Functor (Doubled f) where
--   fmap = fmap . fmap

{-
Now, if this was true, and you had a value of type [[Int]], and you
wrote '_ <$> xs', there would be two possible types of the function
argument: 'Int -> Int' and '[Int] -> [Int]', depending on whether 'xs'
had type '[[Int]]' or 'Doubled [] Int'.  But it has both, so how do
you decide?

Instead, we can introduce a 'newtype' to adapt a function to 'NT'.
That's because, unlike the 'type' keyword, the 'newtype' keyword
actually creates a genuine type.

    Exercise C3: Finish these newtype definitions.
-}

{- DELETE THIS LINE
newtype ForJoin ? = ForJoin {runForJoin :: ...}
joinNT = NT (join . runForJoin)
newtype ForReturn ? = ForReturn {runForReturn :: ...}
returnNT = NT (return . runForReturn)
newtype ForSequence ? = ForSequence {runForSequence :: ...}
sequenceNT = NT (ForSequence . sequenceA . runForSequence)
DELETE THIS LINE -}

{-
    What are the types of the resulting NT values?

    Exercise C4: In reality, the above code only needs two newtypes.
    Which one can be eliminated?

    Exercise C5: The necessary newtypes are already defined in
    Haskell.  Can you swap them in?
-}

-- C.hs ends here
