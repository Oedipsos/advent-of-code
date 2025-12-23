{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)
import Debug.Trace (traceShowId)
import Data.Traversable (for)

type Id = Int
type Range = (Id, Id)

-- pairCombination :: [a] -> [(a, a)]
-- pairCombination [] = []
-- pairCombination (x:xs) = [(x, y) | y <- xs] ++ pairCombination xs

idInRange :: [Range] -> Id -> Bool
idInRange rs i = or [l <= i && i <= h | (l, h) <- rs]

concatRanges :: [Range] -> Range -> [Range]
concatRanges [] r = [r]
concatRanges (r@(l1,h1):rs) r'@(l2,h2) 
    | doesOverlap = concatRanges rs r''
    | otherwise   = r : concatRanges rs r'
    where
        r'' = (min l1 l2, max h1 h2)
        doesOverlap = (min h1 h2 - max l1 l2 + 1) >= 0


main = do
    content :: T.Text <- T.stripEnd <$> TIO.readFile "inputs/Day05.txt"
    let ranges :: [Range] = let toTuple (a:b:[]) = (a, b)
                 in map (toTuple . map (read . T.unpack) . T.splitOn "-") . T.lines . fst . T.breakOn "\n\n" $ content
    let ids :: [Id] = map (read . T.unpack) . T.lines . snd . T.breakOnEnd "\n\n" $ content



    let validIds1 :: [Id] = filter (idInRange ranges) ids

    print $ [length, sum] <*> [validIds1]

    let noOverlapRanges = foldl concatRanges [] ranges
    let totalValidIds2 = let lengths = sum . map ((+1) . ((-) <$> snd <*> fst))
                         in  lengths noOverlapRanges

    print totalValidIds2