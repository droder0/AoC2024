{-# LANGUAGE ViewPatterns #-}
module Day.Day5 (
    part1,
    part2
) where
import Data.Bifunctor (Bifunctor(bimap))
import Data.Set (Set)
import Data.Set qualified as S
import Util (split)
import Data.List (sortOn, sortBy)
import Data.Bool (bool)

parseOrd :: [String] -> Set (Int, Int)
parseOrd = S.fromList . map (bimap read (read . tail) . break (=='|'))

parsePages :: [String] -> [[Int]]
parsePages = map (map read . split (==',')) . tail -- tail because of ""

inOrder :: Set (Int,Int) -> [Int] -> Bool
inOrder ord = all (`S.member` ord) . (zip <*> tail)

middle :: [a] -> a
middle = (!!) <*> ((`div` 2) . length)

part1 :: String -> String
part1 inp = 
    let (parseOrd -> ord, parsePages -> pages) = break (=="") $ lines inp
        filtered = filter (inOrder ord) pages
    in  show $ sum $ map middle filtered


-- This assumes there is a complete ordering. True for my input but not guaranteed
part2 :: String -> String
part2 inp = 
    let (parseOrd -> ord, parsePages -> pages) = break (=="") $ lines inp
        wrong = filter (not . inOrder ord) pages
        sorted = map (sortBy (curry (bool LT GT . (`S.member` ord)))) wrong
    in  show $ sum $ map middle sorted