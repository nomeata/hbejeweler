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

import Data
import Strategy

main = do
	we <- readPlayerDat "player.dat"
	they <- readPlayerDat "opponent.dat"
	gamefield <-  readGamefieldDat "gamefield.dat"
	let gs = GameSituation We we they gamefield
	let move = chooseMove gs
	saveMove "result.dat" move
