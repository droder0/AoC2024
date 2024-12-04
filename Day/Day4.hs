{-# LANGUAGE ViewPatterns #-}
module Day.Day4 (
    part1,
    part2
) where
import Data.List (transpose)
import Util (windowed)


countXmas :: String -> Int
countXmas ('X':'M':'A':xs@('S':_)) = 1 + countXmas xs -- S could be start of a SAMX
countXmas ('S':'A':'M':xs@('X':_)) = 1 + countXmas xs
countXmas (_:xs) = countXmas xs
countXmas [] = 0

diagonals :: [[Char]] -> [[Char]]
diagonals xss = transpose $ zipWith (++) (iterate (' ':) []) xss 

part1 :: String -> String
part1 (lines -> inp) = show $ sum $ map countXmas (inp ++ transpose inp ++ diagonals inp ++ diagonals (reverse inp))

isXmas :: [[Char]] -> Bool
isXmas [[a1,_,a3],[_,'A',_],[c1,_,c3]]
    | all (`elem` "MS") [a1,a3,c1,c3]
    , a1 /= c3
    , a3 /= c1 
    = True
isXmas _ = False

windowed3x3 :: [[Char]] -> [[[Char]]]
windowed3x3 xss = concatMap (foldr (zipWith (:) . windowed 3) (repeat [])) $ windowed 3 xss

part2 :: String -> String
part2 = show . length . filter isXmas . windowed3x3 . lines