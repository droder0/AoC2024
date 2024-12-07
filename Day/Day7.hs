module Day.Day7 (
    part1,
    part2
) where
import Text.ParserCombinators.ReadP (ReadP, char, string, (+++), eof, (<++), manyTill, readP_to_S, get)
import Util (readP)
import Data.Functor (void, ($>))
import Control.Monad (guard)

mul :: ReadP (Int -> Int)
mul = do
    char ' '
    x <- readP
    return (*x)

add :: ReadP (Int -> Int)
add = do
    char ' '
    x <- readP
    return (+x)

parseLine :: ReadP (Int -> Int) -> ReadP Int
parseLine ops = do
    target <- readP
    string ": "
    n <- readP
    ns <- manyTill ops (void (char '\n') <++ eof)
    let result = foldl (flip id) n ns
    guard (result == target)
    return target

part1 :: String -> String
part1 = show . sum . map fst . concatMap (take 1 . readP_to_S (parseLine (add +++ mul))) . lines

cct :: ReadP (Int -> Int)
cct = do
    char ' '
    y <- readP @Int
    return (\x -> read $ show x ++ show y)

part2 :: String -> String
part2 = show . sum . map fst . concatMap (take 1 . readP_to_S (parseLine (add +++ mul +++ cct))) . lines