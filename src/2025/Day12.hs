import Data.List.Split (splitOn)
import Data.Traversable (for)

doesFit :: String -> Bool
doesFit line = area >= totalBoxes
    where
        (grid:boxes:[]) = splitOn ":" line
        area :: Int = product . map ((`div` 3) . read) . splitOn "x" $ grid
        totalBoxes :: Int = sum . map read . words $ boxes

main = do
    content <- lines <$> readFile "inputs/Day12.txt" 
    let result1 = length . filter doesFit $ content

    print result1
