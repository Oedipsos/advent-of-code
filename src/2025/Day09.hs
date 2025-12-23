import Data.List.Split (splitOn)

rectangleVolume x y = product $ zipWith (\a b -> 1 + abs (a-b)) x y

data Edges = 
  {
    xI :: Int
    xF :: Int
    yI :: Int
    yF :: Int
}

pairs []     = []
pairs (x:xs) = [(x, x') | x' <- xs] ++ pairs xs

pairsWith _ []     = []
pairsWith f (x:xs) = [f x x' | x' <- xs] ++ pairsWith f xs


main = do
    let test = False
    points <- map (map (read :: String -> Int) . splitOn ",") . lines
          <$> readFile (if test then "inputs/Day09.test.txt" else "inputs/Day09.txt")

    let result1 = maximum . map (uncurry rectangleVolume) . pairs $ points

    print result1

cabal install megaparsec
