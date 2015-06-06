{-# LANGUAGE RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-unused-matches #-}
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

module NT.A
  ( NT(..)
  ) where

import Control.Monad.Trans
import Data.Maybe

{-
What are we talking about?

We won't be talking about all natural transformations, but those of a
particular type, because that type is interesting to talk about in
Haskell.

All functions of type forall a. f a -> g a, for some f and g.

listToMaybe :: [a] -> Maybe a (f = [], g = Maybe)
reverse :: [a] -> [a] (f = [], g = [])
lift :: m a -> t m a (f = m, g = t m, MonadTrans t, Monad m)
liftIO :: IO a -> m a (f = IO, g = m, MonadIO m)
(. length) :: (Int -> a) -> [t] -> a (f = (->) Int, g = (->) [t])

We're going to use the shorthand "natural transformation" for this
type.  We can create a newtype for it, but remember that nothing
really changes about the value when you wrap or unwrap in a newtype,
so it is only for convenience and dealing with Haskell type rules.
-}

newtype NT f g = NT {runNT :: forall a. f a -> g a}

{-
Now you can try expressions like this, to see if NT can pick out
the f and g involved.

λ> :t NT lift
NT lift :: (MonadTrans t, Monad m) => NT m (t m)

    Exercise A1: Do the same for the other functions mentioned above.
    What are their inferred 'f' and 'g'?

    Exercise A2: What are the kinds of the 'f' and 'g' arguments to
    the 'NT' type constructor?  What about the 'a' after the 'forall'?
-}

-- A.hs ends here
