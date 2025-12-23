import Data.Traversable (for)
import Data.List (sortBy, sort, concat, findIndex)
import Data.List.Split (splitOn)

type Node = [Float]
type Circuit = [Node]
type Network = [Circuit]


euclidianDistance = sqrt . sum . map (^2) . difference
    where difference = uncurry $ zipWith (-) 

pairCombinationWith _ [] = []
pairCombinationWith f (x:xs) = [((x, y), f x y) | y <- xs] ++ pairCombinationWith f xs

addToNetwork [] (x,y) = [[x,y]]
addToNetwork net@(n:ns) (x,y) = case (x `elem` n, y `elem` n) of
    (True, True)   -> net
    (True, False)  -> n':ns'
    (False, True)  -> n'':ns''
    (False, False) -> n : addToNetwork ns (x,y)
    where
        ysub = concat $ filter (elem y) ns
        n'    = n ++ if null ysub then [y] else ysub
        ns'   = filter (not . elem y) ns
        xsub = concat $ filter (elem x) ns
        n''   = n ++ if null xsub then [x] else xsub
        ns''  = filter (not . elem x) ns

main = do
    let test = False
    points <- map (map (read :: String -> Float) . splitOn ",") . lines
          <$> readFile (if test then "inputs/Day08.test.txt" else "inputs/Day08.txt")

    let linkableBoxes = let distance x y = compare (snd x) (snd y) 
                        in sortBy distance . pairCombinationWith (curry euclidianDistance) $ points
    
    let networks = foldl addToNetwork [] $ map fst $ take (if test then 10 else 1000) linkableBoxes

    let result1 = product . take 3 . reverse . sort . map length $ networks

    print result1

    -- Part 2

    let n = length points
    let finalNetwork = scanl addToNetwork [] $ map fst linkableBoxes
    
    let (Just ix) = findIndex (\x -> length x == 1 && length (x !! 0) == n) finalNetwork 
   
    let result2 = let (x1, x2) = head . uncurry zip . fst . (!!(ix-1)) $ linkableBoxes
                  in x1 * x2
    print result2



