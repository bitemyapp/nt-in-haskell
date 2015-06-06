{-# LANGUAGE RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fdefer-type-errors -fno-warn-orphans -fno-warn-unused-imports -fno-warn-unused-matches #-}
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

module NT.D where

import Control.Arrow
import Control.Category
import Data.Functor.Product
import Data.Functor.Sum
import NT.A
import Prelude hiding ((.), id)

{-
How can you combine natural transformations?

Well, they behave a lot like functions.  We're going to use the
reordering of type parameters granted by the 'NT' newtype to derive
some interesting combinators.

Since GHC 7.8, we can provide instance 'Category' for 'NT' in the same
way it's provided for '(->)'.

    Exercise D1: Solve these holes:
-}

instance Category NT where
  id = NT _catId
  NT f . NT g = NT _catDot

product :: NT f g -> NT h k -> NT (Product f h) (Product g k)
NT fg `product` NT hk = NT (uncurry Pair . _product . runProduct)
  where runProduct (Pair a b) = (a, b)

sum :: NT f g -> NT h k -> NT (Sum f h) (Sum g k)
NT fg `sum` NT hk = NT (mksum . _sum . runSum)
  where runSum (InL a) = Left a
        runSum (InR b) = Right b
        mksum = either InL InR

{-
There are more combinations you could try, but let's move on.
-}

-- D.hs ends here
