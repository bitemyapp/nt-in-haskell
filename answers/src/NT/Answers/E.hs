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

module NT.Answers.E where

import Control.Category
import NT.E
import Prelude hiding ((.), id)

-- Exercise E1
instance Category c => Category (NTC c) where
  id = NTC id
  NTC f . NTC g = NTC (f . g)

-- Exercise E2
instance Category c => Category (IsoC c) where
  id = IsoC id id
  IsoC f h . IsoC g k = IsoC (f . g) (k . h)

-- Exercise E3
{-
λ> :t \(NTC (IsoC f g)) -> (f, g)
... forall a. t2 (t a) (t1 a), t2 (t1 a) (t a)

λ> :t \(IsoC (NTC f) (NTC g)) -> (f, g)
... forall a. t2 (t a) (t1 a), forall b. t2 (t1 b) (t b)
-}
