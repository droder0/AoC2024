module Day.Day2 (
    part1,
    part2
) where

import Data.List (inits, tails)

part1 :: String -> String
part1 = show . length . filter (
    \xs -> all (<0) xs && all (>= -3) xs ||
        all (>0) xs && all (<=3) xs)
    . map ((>>=) tail (zipWith (-)) . map read . words) . lines

all1Missing :: [a] -> [[a]]
all1Missing xs = zipWith (++) (inits xs) (tail $ tails xs)

part2 :: String -> String
part2 = show . length . filter (any (\xs -> all (<0) xs && all (>= -3) xs || all (>0) xs && all (<=3) xs)) . map (map ((>>=) tail (zipWith (-))) . all1Missing . map read . words) . lines