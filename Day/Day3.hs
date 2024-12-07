{-# LANGUAGE BlockArguments #-}
module Day.Day3 (
    part1,
    part2
) where

import Text.ParserCombinators.ReadP
    ( ReadP, char, get, many1, readP_to_S, string, (<++), eof, manyTill)
import Data.Char (isDigit)
import Text.ParserCombinators.ReadPrec (readPrec_to_P)
import Text.Read (Read(readPrec))
import Data.Functor (($>), void)
import Util (readP)

multiplication :: ReadP Int
multiplication = do
    string "mul("
    n1 <- readP
    char ','
    n2 <- readP
    char ')'
    return $ n1*n2

part1 :: String -> String
part1 = show . fst . head . readP_to_S do
    muls <- many1 (multiplication <++ (get $> 0))
    eof
    return $ sum muls

don't :: ReadP Int
don't = do
    string "don't()"
    manyTill get (void (string "do()") <++ eof)
    return 0

part2 :: String -> String
part2 = show . fst . head . readP_to_S do
    muls <- many1 (multiplication <++ don't <++ (get $> 0))
    eof
    return $ sum muls