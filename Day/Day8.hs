{-# LANGUAGE ViewPatterns #-}
module Day.Day8 (
    part1,
    part2
) where
import Data.Set (Set)
import Data.Set qualified as S
import Data.Map (Map)
import Data.Map qualified as M
import Util (indexedGridBounds)

type Position = (Int,Int)
type Vector = (Int,Int)

move :: Position -> Vector -> Position
move (x,y) (dx,dy) = (x+dx, y+dy)

diff :: Position -> Position -> Vector
diff (x,y) (x',y') = (x'-x,y'-y)

inverse :: Vector -> Vector
inverse (dx,dy) = (-dx,-dy)

type Antennas = Map Char [Position]

insertAntenna :: Char -> Position -> Antennas -> Antennas
insertAntenna '.' p = id
insertAntenna c p = M.alter (Just . maybe [p] (p:)) c

findAntinodes :: (Position -> Position -> [Position]) -> [Position] -> Set Position
findAntinodes _ [] = S.empty
findAntinodes f (x:xs) = S.fromList (concatMap (f x) xs) `S.union` findAntinodes f xs

inBounds :: Int -> Int -> Position -> Bool
inBounds xBound yBound (x,y) = x >= 0 && x < xBound && y>=0 && y < yBound

part1 :: String -> String
part1 (indexedGridBounds . lines -> (indexed,xBound,yBound)) =
    let antennas = foldl (flip (uncurry insertAntenna)) M.empty indexed

        pairAntinodes :: Position -> Position -> [Position]
        pairAntinodes p p2 =
            let v = diff p p2
            in  filter (inBounds xBound yBound) [move p (inverse v),move p2 v]

        antinodes = M.foldrWithKey (\_ ps s -> S.union s $ findAntinodes pairAntinodes ps) S.empty antennas
    in  show $ S.size antinodes

part2 :: String -> String
part2 (indexedGridBounds . lines -> (indexed,xBound,yBound)) =
    let antennas = foldl (flip (uncurry insertAntenna)) M.empty indexed

        pairAntinodes :: Position -> Position -> [Position]
        pairAntinodes p p2 =
            let v = diff p p2
            in  takeWhile (inBounds xBound yBound) (iterate (`move` v) p) ++ p:takeWhile (inBounds xBound yBound) (iterate (`move` inverse v) p)

        antinodes = M.foldrWithKey (\_ ps s -> S.union s $ findAntinodes pairAntinodes ps) S.empty antennas
    in  show $ S.size antinodes