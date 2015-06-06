{-# LANGUAGE RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fdefer-type-errors -fno-warn-unused-imports -fno-warn-unused-matches #-}
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

module NT.E where

import Control.Category
import Prelude hiding ((.), id)

{-
Is NT in its most general form?

Not quite.  Let's look at it again.

newtype NT f g = NT (forall a. f a -> g a)

It's almost entirely abstracted, so many type variables!  But there's
one concrete thing left, the (->).  Let's abstract over it, so it
generally depicts a relation between 'f a' and 'g a', rather than a
Haskell function.
-}

newtype NTC c f g = NTC (forall a. c (f a) (g a))

{-
    Exercise E1: This one has an interesting 'Category' instance.
    Solve these holes by adding the necessary constraint on 'c'.
-}

{- DELETE THIS LINE
instance ??? c => Category (NTC c) where
  id = NTC _catIdNTC
  NTC f . NTC g = NTC _catDotNTC
DELETE THIS LINE -}

{-
'NTC (->)', like 'NT', represents a natural transformation, though
'NTC c' does not represent a natural transformation for all categories
'c'.  However, our real concern is universal quantification, and this
structure is generalization of 'NT' in one dimension.

Why abstract over the '->' in this dimension?  Well, there are other
interesting 'Category' types that combine with the new one in
interesting ways.  For example, isomorphisms.
-}

data IsoC c a b = IsoC (c a b) (c b a)

{-
    Exercise E2: Implement 'Category' for 'IsoC'.

    Exercise E3: What does an 'NTC' of 'IsoC' represent?  What about
    an 'IsoC' of 'NTC'?
-}

-- E.hs ends here
