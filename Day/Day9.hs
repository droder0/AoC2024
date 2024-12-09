module Day.Day9 (
    part1,
    part2
) where

import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.List (intersperse)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Debug.Trace (traceShowId)

reOrder :: Seq (Maybe Int) -> Seq Int
reOrder Empty = Empty
reOrder (Just a :<| s) = a :<| reOrder s
reOrder (Nothing :<| (s :|> Just a)) = a :<| reOrder s
reOrder (Nothing :<| (s :|> Nothing)) = reOrder (Nothing :<| s)
reOrder (Nothing :<| Empty) = Empty

part1 :: String -> String
part1 inp =
    let values = intersperse Nothing $ map Just [0 ..]
        decompressed = concat $ zipWith (replicate . digitToInt) inp values
        reordered = toList $ reOrder $ Seq.fromList decompressed

    in show $ sum $ zipWith (*) reordered [0..]

type Size = Int
data DiskSpace = File Int Size | Block Size deriving (Show, Eq)

size :: DiskSpace -> Int
size (File _ s) = s
size (Block s) = s

insertFile :: DiskSpace -> Seq DiskSpace -> Maybe (Seq DiskSpace)
insertFile _ Empty = Nothing
insertFile (Block _) _ = Nothing
insertFile f (f2@(File _ _) :<| d) = (f2 :<|) <$> insertFile f d
insertFile f@(File _ fs) (Block bs :<| d)
    | fs == bs = Just (f :<| d)
    | fs < bs = Just (f :<| Block (bs-fs) :<| d)
    | otherwise = (Block bs :<|) <$> insertFile f d

reOrder2 :: Seq DiskSpace -> Seq DiskSpace
reOrder2 Empty = Empty
reOrder2 (d :|> f) = case insertFile f d of
    Nothing -> reOrder2 d :|> f
    Just d' -> reOrder2 d' :|> Block (size f)

expandFile :: DiskSpace -> [Int]
expandFile (File i s) = replicate s i
expandFile (Block s) = replicate s 0

part2 :: String -> String
part2 inp =
    let values = intersperse Block $ map File [0 ..]
        disk = zipWith (flip id . digitToInt) inp values
        reordered = concatMap expandFile $ toList $ reOrder2 $ Seq.fromList disk
    in  show $ sum $ zipWith (*) reordered [0..]