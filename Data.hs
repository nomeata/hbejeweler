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

module Data where

import Data.Array.IArray
import Data.Maybe
import Data.Ord
import Data.Function
import Data.List

data Color = Red | Yellow | Green | Purple | Blue deriving (Eq, Show)

data Stone = Bomb Int | Colored Color | Unknown deriving (Eq, Show)

data PlayerStats = PlayerStats
	{ hitpoints :: Int
	, shield :: Int
	, collectedRed :: Int
	, collectedYellow :: Int
	, collectedGreen :: Int
	, collectedPurple :: Int
	} deriving Show

type GameField = Array (Int, Int) Stone

data Turn = We | They deriving (Eq, Show)

data GameSituation = GameSituation
	{ turn :: Turn
	, atTurn :: PlayerStats
	, opponent :: PlayerStats
	, gameField :: GameField
	} deriving Show

type Move = ((Int,Int), (Int,Int))

readPlayerDat :: String -> IO PlayerStats
readPlayerDat fileName =
     do file <- readFile fileName
	let [hp,s,r,y,g,p] = map read $ lines file
        return $ PlayerStats hp s r y g p

readGamefieldDat :: String -> IO GameField
readGamefieldDat filename = 
     do file <- readFile filename
        return $ listArray ((0,0),(9,9)) $ mapMaybe c $ words $ concat $ reverse $ lines file
  where c = flip lookup $ stoneChars

saveMove :: String -> Move -> IO ()
saveMove filename ((x1,y1), (x2,y2)) =
	writeFile filename $ unwords $ map show $ [y1,x1,y2,x2]

applyMove :: Move -> GameSituation -> GameSituation
applyMove move gs = GameSituation turn' atTurn' opponent' gameField'
  where	flippedGameField = flipStones (gameField gs) move
	(gameField', sequences) = removeTiles flippedGameField []
	atTurnCounted = foldr countStones (atTurn gs) sequences
	(atTurnAttacked, damage, noFlip) = attack atTurnCounted
	bombDamage = sum $ mapMaybe fromBomb $ concat sequences
	(atTurn', opponent') = (if noFlip then id else flipTuple) $ 
	                       (atTurnAttacked, applyDamage (opponent gs) (damage + bombDamage))
	turn' = (if noFlip then id else flipTurn) (turn gs)

flipStones gf (i1,i2) = gf // [(i1, gf ! i2),(i2, gf ! i1)]

possibleMoves gf = filter works $ filter valid $ allMoves
  where allMoves = concat [ [((i,j),(i+1,j)), ((i,j),(i,j+1))] | (i,j) <- range ((0,0),(9,9)) ]
        valid (_,(i,j)) = i < 10 && j < 10 -- first coordinate is always correct
	works move@(i1,i2) = let gf' = flipStones gf move
                             in  lookAround i1 gf' || lookAround i2 gf'
{-      The following code is a bit more haskellish, but too slow. Replacing
 -      it with the code below give a 30% time boost.
        lookAround (i,j) gf' = any ((>=3).length) $
		(groupBy combineable $ [ gf' ! (i',j) | i' <- range (max 0 (i-2), min 9 (i+2)) ]) ++
 		(groupBy combineable $ [ gf' ! (i,j') | j' <- range (max 0 (j-2), min 9 (j+2)) ])
-}      
	{-  A
	    B
	  FGCHK
	    D
	    E -}
	lookAround (i,j) gf' = let
		a = if i < 8 then gf' ! (i+2,j) else Unknown
		b = if i < 9 then gf' ! (i+1,j) else Unknown
		c =               gf' ! (i,j)
		d = if i > 0 then gf' ! (i-1,j) else Unknown
		e = if i > 1 then gf' ! (i-2,j) else Unknown
		f = if j > 1 then gf' ! (i,j-2) else Unknown
		g = if j > 0 then gf' ! (i,j-1) else Unknown
		h = if j < 9 then gf' ! (i,j+1) else Unknown
		k = if j < 8 then gf' ! (i,j+2) else Unknown
		ab = a `combineable` b
		bc = b `combineable` c
		cd = c `combineable` d
		ch = c `combineable` h
		de = d `combineable` e
		fg = f `combineable` g
		gc = g `combineable` c
		hk = h `combineable` k
		in bc && (ab || cd) || cd && de ||
                   gc && (fg || ch) || ch && hk

removeTiles :: GameField -> [[Stone]] -> (GameField, [[Stone]])
removeTiles field sequences =
        let indexed = indexedTiles field
	    removedIndizes = nub $ map fst $ concat indexed
	    sequences' = map (map snd) $ indexed
	    field' = foldr dropTiles field removedIndizes
	in if null indexed then (field, sequences)
                           else removeTiles field' (sequences' ++ sequences)
  where dropTiles (i,j) f = f // ([ ((i',j), f ! (i'+1,j)) | i' <- range (i,8)] ++ [ ((9,j),Unknown) ])

indexedTiles field = concatMap findInteresting $ rows ++ cols
  where rows = [ [ ((i,j),field ! (i,j)) | i <- range (0,9)] | j <- range (0,9) ]
	cols = [ [ ((i,j),field ! (i,j)) | j <- range (0,9)] | i <- range (0,9) ]
        findInteresting = filterChains . groupBy (combineable `on` snd)

{- filterChains = filter((>=3).length), but optimized -}
filterChains [] = []
filterChains ([]:xs) = filterChains xs
filterChains ([_]:xs) = filterChains xs
filterChains ([_,_]:xs) = filterChains xs
filterChains (x:xs) = x:filterChains xs

attack ps@(PlayerStats
        { collectedRed = r, collectedYellow = y, collectedGreen = g, collectedPurple = p }) =
	(ps { collectedRed = r `mod` 15, collectedYellow = y `mod` 15,
              collectedGreen = g `mod` 15, collectedPurple = p `mod` 15 },
        10 * (r `div` 15) + 3 * (g `div` 15) + 6 * (y `div` 15),
        p >= 15)

applyDamage ps damage | damage <= shield ps = ps { shield = shield ps - damage }
		      | otherwise           = ps { shield = 0,
                                                   hitpoints = hitpoints ps + shield ps - damage}

countStones seq@(Colored Red:_) ps = ps { collectedRed = length seq + collectedRed ps }
countStones seq@(Colored Green:_) ps = ps { collectedGreen = length seq + collectedGreen ps }
countStones seq@(Colored Yellow:_) ps = ps { collectedYellow = length seq + collectedYellow ps }
countStones seq@(Colored Purple:_) ps = ps { collectedPurple = length seq + collectedPurple ps }
countStones seq@(Colored Blue:_) ps = ps { shield = min 15 $ shield ps + length seq }
countStones _ ps = ps -- ignore bombs

flipTurn We = They
flipTurn They = We

combineable (Colored c) (Colored c') = c == c'
combineable (Bomb _) (Bomb _) = True
combineable _ _ = False

fromBomb (Bomb i) = Just i
fromBomb _ = Nothing

showField gf = unlines $
	[ unwords [ stoneToChar (gf ! (i,j)) | j <- range (0,9) ]
        | i <- reverse (range (0,9)) ]
  where stoneToChar = fromJust . flip lookup (map flipTuple stoneChars)

stoneChars = [("R", Colored Red),("Y",Colored Yellow),
              ("G",Colored Green),("B",Colored Blue),
              ("L",Colored Purple), ("?", Unknown)] ++
              [(show i, Bomb i) | i <- [1..5]]

flipTuple (x,y) = (y,x)
