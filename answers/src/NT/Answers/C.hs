{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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

module NT.Answers.C where

import NT.A
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Traversable (sequenceA)

{-
Exercise C1:

λ> :t NT join

<interactive>:1:4-7: Warning:
    Couldn't match type ‘a’ with ‘f a’
      ‘a’ is a rigid type variable bound by
          a type expected by the context: f a -> f a at <interactive>:1:1
    Expected type: f a -> f a
      Actual type: f (f a) -> f a
    In the first argument of ‘NT’, namely ‘join’
    In the expression: NT join

(This would be an error without -fdefer-type-errors.)

Exercise C2:
[], Char
State Int, Int (or StateT Int Identity, Int)
Identity, Int
(->) String, [String]
Const [Int], ()
-}

-- Exercise C3
newtype ForJoin m a = ForJoin {runForJoin :: m (m a)}
joinNT = NT (join . runForJoin)
newtype ForReturn m a = ForReturn {runForReturn :: a}
returnNT = NT (return . runForReturn)
newtype ForSequence f g a = ForSequence {runForSequence :: f (g a)}
sequenceNT = NT (ForSequence . sequenceA . runForSequence)

-- Exercise C4
-- ForJoin is unneeded.
joinNT2 = NT (join . runForSequence)

-- Exercise C5
-- Compose and Identity are already defined.  We can replace
-- ForSequence with Compose, and ForReturn with Identity.
joinNT' = NT (join . getCompose)
returnNT' = NT (return . runIdentity)
sequenceNT' = NT (Compose . sequenceA . getCompose)
