module Sudoku where

import Data.Array
import Data.Char
import Data.List
import Data.Tree

p :: Sudoku
p = listArray ((1,1,1,1),(3,3,3,3)) $ digitToInt <$> "200370009009200007001004002050000800008000900006000040900100500800007600400089001"

type Coord  = (Int,Int,Int,Int) 
type Sudoku = Array Coord Int

children :: Coord -> Sudoku -> [Sudoku]
children c@(i,j,k,l) s = [ s//[(c,g)] | g <- [1..9] \\ neighbors ] where
        neighbors = (s!) <$> [ (i,j,k,l) | k <- rk, l <- rl ] ++
                             [ (i,j,k,l) | i <- ri, j <- rj ] ++
                             [ (i,j,k,l) | j <- rj, l <- rl ]
        ((i0,j0,k0,l0),(i1,j1,k1,l1)) = bounds s
        [ri,rj,rk,rl] = range <$> [(i0,i1),(j0,j1),(k0,k1),(l0,l1)]

spaces :: Sudoku -> [Coord]
spaces s = [ c | (c,0) <- assocs s ]

stree :: Sudoku -> Tree (Sudoku, [Coord])
stree s = unfoldTree gen (s, spaces s) where
        gen t@(s, c:cs) = (t, [ (s',cs) | s' <- children c s ])
        gen t = (t, [])

solve :: Sudoku -> [Sudoku]
solve = fmap fst . filter done . flatten . stree where
        done = null . snd
