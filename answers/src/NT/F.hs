{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, PolyKinds #-}
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

module NT.F where

import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import NT.A

{-
Naturally transforming monad transformers

It's easy to add another transformer on top of a monad transformer
stack; there's even an abstraction to do it generally, 'lift'.  But
for adding another transformer underneath an existing layer, there's
no definite abstraction.

There is an existing menagerie of lifting methods for talking about
the 'm' underneath a transformer, most named "map", and scattered in
various modules with various names and different signatures.

mapStateT
mapMaybeT
mapReaderT
hoistF

    Exercise F1: What is the type of 'mapStateT lift'?  What about
    'mapStateT (mapMaybeT lift)'?

Using NT, or the higher-ranked form, we can build a unifying
abstraction for transforming the 'm' in 't m' for 'MonadTrans' 't',
just like 'MonadTrans' adds that 't' to the 'm'.
-}

class MonadTrans t => MonadHoist t where
  hoist :: NT f g -> t f a -> t g a

{-
    Exercise F2: Implement 'MonadHoist' for 'StateT' and 'MaybeT'.

    Exercise F3: How can the two expressions in exercise F1 be
    rewritten to take advantage of 'hoist'?

    Exercise F4: Implement 'MonadHoist' for 'ReaderT' and 'F'.

    Exercise F5: Are there monad transformers for which 'MonadHoist'
    *cannot* be instantiated?  Which ones?
-}

-- F.hs ends here
