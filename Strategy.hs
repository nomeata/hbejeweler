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
import Data.Tree.Game_tree.Game_tree
import Data.Tree.Game_tree.Negascout

instance Game_tree GameSituation where
  is_terminal gs =    hitpoints (atTurn gs) <= 0
                   || hitpoints (opponent gs) <= 0
                   || null (children gs)
  children gs = [ applyMove move gs | move <- possibleMoves (gameField gs) ]
  node_value gs = (if We == turn gs then id else negate) $
                  playerValue (atTurn gs) - playerValue (opponent gs)
    where playerValue (PlayerStats h s r y g p) | h <= 0    = -200
                                                | otherwise = 5 * h + s
		   
chooseMove :: GameSituation -> Move
chooseMove gs = fst $ maximumBy (comparing snd) $
                [ (move, snd (alpha_beta_search (applyMove move gs) 3))
                  | move <- possibleMoves (gameField gs) ]


