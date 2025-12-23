{- cabal:
build-depends:
    base,
    containers
-}

import Control.Applicative
import Data.List (concat, findIndices, nub)
import qualified Data.Map as M


countSplits :: [Int] -> [String] -> Int
countSplits _ []        = 0
countSplits beam (l:ls) = (splits + countSplits beam' ls)
    where
        splits = length . filter id $ (==) <$> beam <*> splitters
        splitters = findIndices (=='^') l
        beam' = nub . concat $ [if x `elem` splitters then [x+1, x-1] else [x] | x <- beam]


simulateBeam :: M.Map Int Int -> [String] -> M.Map Int Int
simulateBeam beam []     = beam
simulateBeam beam (l:ls) = simulateBeam beam' ls
    where
        splitters = findIndices (=='^') l
        beam' = M.fromListWith (+) 
              . concat 
              . map (\(k, v) -> if k `elem` splitters then [(k-1, v), (k+1, v)] else [(k, v)])
              $ M.toList beam

main = do

    let test = False
    content <- lines <$> readFile (if test then "inputs/Day07.test.txt" else "inputs/Day07.txt")
    let beam = findIndices (=='S') $ head content

    let result1 = countSplits beam (drop 2 content)

    print result1

    let beamMap = M.fromList $ map (\x -> (x, 1)) beam
    let result2 = sum . M.elems $ simulateBeam beamMap (drop 2 content)

    print result2