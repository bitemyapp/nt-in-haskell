{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall -fdefer-type-errors -fno-warn-orphans
                -fno-warn-missing-signatures
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

module NT.Answers.F where

import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import NT.A
import NT.F

{-
Exercise F1
λ> :t mapStateT lift
mapStateT lift
  :: (MonadTrans t, Monad m) => StateT s m b -> StateT s (t m) b
λ> :t mapStateT (mapMaybeT lift)
mapStateT (mapMaybeT lift)
  :: (MonadTrans t, Monad m) =>
     StateT s (MaybeT m) b -> StateT s (MaybeT (t m)) b
-}

-- Exercise F2
instance MonadHoist (StateT s) where
  hoist (NT f) = mapStateT f

instance MonadHoist MaybeT where
  hoist (NT f) = mapMaybeT f

-- Exercise F3
hoistST = mapStateT lift `asTypeOf` hoist (NT lift)

hoistSTMT = mapStateT (mapMaybeT lift)
    `asTypeOf` hoist (NT (hoist (NT lift)))

-- Exercise F4
instance MonadHoist (ReaderT r) where
  hoist (NT f) = mapReaderT f

instance MonadHoist F where
  hoist (NT f) = hoistF f

{-
Exercise F5:

ContT (from transformers) and Codensity (from kan-extensions) have 'm'
in an invariant position; that is, you require an (NT m n) *and* (NT n
m) to transform 'm' to 'n'.

While 'f' in 'F' appears in an argument position, it appears in an
argument's argument position; the double negative becomes positive,
and the 'f' is covariant again.
-}
