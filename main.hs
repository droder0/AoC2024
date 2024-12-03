{-# LANGUAGE LambdaCase #-}
module Main where
import Day.Day1 (part1, part2)
import Day.Day2 (part1, part2)
import Day.Day3 (part1, part2)

days :: [(String -> String, String -> String)]
days = [
    (Day.Day1.part1, Day.Day1.part2),
    (Day.Day2.part1, Day.Day2.part2),
    (Day.Day3.part1, Day.Day3.part2)
    ]

input :: String -> IO String
input i = readFile ("inputs\\" ++ i ++ ".txt")

part :: String -> String -> String -> String
part "1" a = fst (days !! (read a - 1))
part "2" a = snd (days !! (read a - 1))
part _ a = const "No such part!"

main :: IO ()
main = do
    putStrLn "What day?"
    day <- getLine
    putStrLn "What part?"
    pt <- getLine
    putStrLn "'sample' or 'input'?"
    inp <- getLine >>= \case
        "sample" -> readFile "sample.txt"
        "input" -> input day
    putStrLn (part pt day inp)