import Debug.Trace (trace)

debug = flip trace

combineRep :: Int -> [a] -> [[a]]
combineRep k [] = []
combineRep k la@(l:ls)
    | k <= 0    = [[]]
    | otherwise = map (l:) (combineRep (k-1) la) ++ combineRep k ls


tails :: [a] -> [[a]]
tails [] = [[]]
tails lt@(_:ls) = lt : (tails ls)

inits :: [a] -> [[a]]
inits = map reverse . scanl (flip (:)) []

splitEverywhere :: [a] -> [([a], a, [a])]
splitEverywhere xs = 
    map 
        (\(ys, ys') -> 
            case ys' of
                z:zs -> (ys, z, zs)
                [] -> error "empty list")
        (init (zip (inits xs) (tails xs)))



removeEach :: [a] -> [(a, [a])]
removeEach = map (\(ys,y,ys') -> (y, ys++ys')) . splitEverywhere


combine :: (Show a) => Int -> [a] -> [[a]]
combine k ls = 
    case compare k 0 of
        LT -> []
        EQ -> [[]]
        GT -> [l:c | (l, ls') <- removeEach ls, c <- combine (k-1) ls']


main = do
--     print $ inits "abc"
--     print $ tails "abc"
--     print $ splitEverywhere "abc"
    print $ length $ combineRep 100 [1..11]
