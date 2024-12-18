module Util where
import Data.List (tails)
import Text.ParserCombinators.ReadP (ReadP)
import Text.Read (readPrec_to_P, Read (readPrec))


readP :: Read a => ReadP a
readP = readPrec_to_P readPrec 0

windowed :: Int -> [a] -> [[a]]
windowed n = takeWhile ((==n) . length) . map (take n) . tails

split :: (a->Bool) -> [a] -> [[a]]
split p xs = case break p xs of
    (ys,[]) -> [ys]
    (ys,z:zs) -> ys : split p zs

chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs = case splitAt i xs of
    (ys,[]) -> [ys]
    (ys,zs) -> ys : chunksOf i zs

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix sfx (x:xs)
    | sfx == xs = Just [x]
    | otherwise = fmap (x:) (stripSuffix sfx xs)
stripSuffix sfx [] = Nothing

indexedGridBounds :: [[a]] -> ([(a,(Int,Int))],Int,Int)
indexedGridBounds xs =
    let xBound = length $ head xs
        yBound = length xs
        coords = flip (,) <$> [0 .. yBound-1] <*> [0 .. xBound-1]
    in (zip (concat xs) coords,xBound,yBound)

indexedGrid :: [[a]] -> [(a, (Int, Int))]
indexedGrid = (\(x,_,_)->x) . indexedGridBounds