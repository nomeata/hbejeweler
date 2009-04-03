{-# LANGUAGE FlexibleInstances #-}

-- | Contestant for the Freies Magazin programming contest.
-- Copyright 2009 Joachim Breitner
--
-- This file is part of hbejeweler
--
--  hbejeweler is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.

--  hbejeweler is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with hbejeweler.  If not, see <http://www.gnu.org/licenses/>.

module Strategy where

import Data
import Data.List
import Data.Ord
import Data.Maybe
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

depth = 2

instance Game_tree (GameSituation, Maybe Move) where
  is_terminal t@(gs,_) =    hitpoints (atTurn gs) <= 0
                   || hitpoints (opponent gs) <= 0
                   || null (children t)
  children (gs,_) = [ (applyMove move gs, Just move) | move <- possibleMoves (gameField gs) ]
  node_value (gs,_) = -- (if even depth then id else negate) $ -- work around bug in game-tree?
                  -- (if We == turn gs then id else negate) $
                  playerValue (atTurn gs) - playerValue (opponent gs)
    where playerValue (PlayerStats h s r y g p) | h <= 0    = -2000
                                                | otherwise = 60 * h + 15 * s + 10 * r + 6 * y + 3 * g
		   
chooseMove :: GameSituation -> Move
chooseMove gs = fromJust $ snd $ head $ tail $ fst $ negascout (gs, Nothing) depth


