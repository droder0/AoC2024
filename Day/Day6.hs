{-# LANGUAGE ViewPatterns #-}
module Day.Day6 (
    part1,
    part2
) where
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.List (find)
import Data.Set qualified as S
import Data.Set (Set)
import Debug.Trace (traceShowId)

type Direction = (Int,Int)
type Position = (Int, Int)
 
rotateRight :: Direction -> Direction
rotateRight (x,y) = (-y,x)

move :: Direction -> Position -> Position
move (dx,dy) (x,y) = (x+dx,y+dy)

step :: Set Position -> (Position, Direction, Set Position) -> (Position, Direction, Set Position)
step obstacles (pos, d, visited) = case move d pos of
    pos'
        | S.member pos' obstacles -> step obstacles (pos, rotateRight d, visited)
        | otherwise -> (pos', d, S.insert pos visited)

part1 :: String -> String
part1 (lines -> inp) =
    let indexed = concat $ zipWith (\y xs -> zipWith (\x z -> (z, (x,y))) [0..] xs) [0..] inp
        obstacles = S.fromList $ mapMaybe (\(x,pos) -> guard (x == '#') $> pos) indexed
        guardPos = maybe (0,0) snd (find ((=='^') . fst) indexed)
        guardDirection = (0,-1)
        xBound = length $ head inp
        yBound = length inp
    in   show $ (\(_,_,s) -> S.size s) $ until (\((x,y),_,_) -> x < 0 || x >= xBound || y <0 || y >= yBound) (step obstacles) (guardPos,guardDirection,S.empty)
    -- in  show xBound ++ " " ++ show yBound
    -- in  show guardPos

step2 :: Set Position -> (Position, Direction, Set (Position, Direction)) -> (Position, Direction, Set (Position, Direction))
step2 obstacles (pos, d, visited) = case move d pos of
    pos'
        | S.member pos' obstacles -> step2 obstacles (pos, rotateRight d, S.insert (pos,d) visited)
        | otherwise -> (pos', d, visited)

isLoop :: (Int,Int) -> Set Position -> (Position, Direction, Set (Position, Direction)) -> Bool
isLoop (xBound,yBound) obstacles s@(pos, d, visited) = case step2 obstacles s of
    new@(pos'@(x,y), d', visited')
        | S.member (pos',d) visited -> True
        | x < 0 || x >= xBound || y < 0 || y >= yBound -> False
        | otherwise -> isLoop (xBound,yBound) obstacles  new

part2 :: String -> String
part2 (lines -> inp) =
    let indexed = concat $ zipWith (\y xs -> zipWith (\x z -> (z, (x,y))) [0..] xs) [0..] inp
        obstacles = S.fromList $ mapMaybe (\(x,pos) -> guard (x == '#') $> pos) indexed
        guardPos = maybe (0,0) snd (find ((=='^') . fst) indexed)
        guardDirection = (0,-1)
        xBound = length $ head inp
        yBound = length inp
        allIndices = (,) <$> [0 .. xBound] <*> [0 .. yBound]
    in  show . length . filter (\pos -> isLoop (xBound,yBound) (S.insert pos obstacles) (guardPos, guardDirection, S.empty)) $ allIndices