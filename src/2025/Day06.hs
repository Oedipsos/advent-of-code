{- cabal:
build-depends:
    base
-}

import Data.List (groupBy)
import Data.Traversable (for)
import Control.Applicative

type Operation = (String, [String])

compute :: Operation -> Int
compute ("+",xs) = sum . map read $ xs
compute ("*",xs) = product . map read $ xs
compute (_,_)    = error "Not implemented"


chunks p xs = 
    case dropWhile p xs of
        []  -> []
        xs' -> x : chunks p xs''
            where (x, xs'') = break p xs' 


main = do
    -- Part 1
    content <- filter (/="\n") . lines <$> readFile "inputs/Day06.txt"
    
    let operations1 = map (\x -> (last x, init x)) $ getZipList $ traverse ZipList $ map words content
    let result1 = sum $ map compute operations1
    print result1   
    

    let operands2 = chunks (=="    ") $ getZipList $ traverse ZipList (init content)
    let ops2 = words $ last content
    let operations2 = zipWith (,) ops2 operands2
    let result2 = sum $ map compute operations2

    print result2

    