{-# LANGUAGE RankNTypes, PolyKinds #-}
{-# OPTIONS_GHC -Wall #-}
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

module NT.Answers.A where

{-
Exercise A1:
λ> :t NT listToMaybe
NT listToMaybe :: NT [] Maybe
λ> :t NT reverse
NT reverse :: NT [] []
λ> :t NT liftIO
NT liftIO :: MonadIO m => NT IO m
λ> :t NT (. length)
NT (. length) :: NT ((->) Int) ((->) [a])

Exercise A2:
λ> :k NT
NT :: (k -> *) -> (k -> *) -> *

'f' and 'g' both have kind 'k -> *' (for the same 'k'), and the 'a'
has kind 'k'.
-}
