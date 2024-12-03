module Day.Day1 (
    part1,
    part2
) where

import Data.List (transpose, sort)

part1 :: String -> String
part1 = show . sum . map (abs . foldr1 (-)) . transpose . map sort . transpose . map (map read . words) . lines

part2 :: String -> String
part2 = show . sum . (\[as,bs] -> [a * length (filter (==a) bs) | a <- as]) . transpose . map (map read . words) . lines