inputs/import Debug.Trace (trace)

debug = flip trace

-- importing (?!) operator from Data.List in base-4.21 (currently in 4.17 on my system)

(!?) :: [a] -> Int -> Maybe a

{-# INLINABLE (!?) #-}
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n


type Board = [[Char]]
type Point = (Int, Int)

indicesGrid :: Board -> [Point]
indicesGrid board = [(x, y) | y <- [0..rows-1], x <- [0..cols-1]]
    where (rows, cols) = (length board, length $ board !! 0)


getSquare :: Board -> Point -> Maybe Char
getSquare board (x, y) = return board >>= (!? y) >>= (!? x)

getNeighbours :: Point -> [Point]
getNeighbours (x, y) = [(x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], (dx, dy) /= (0, 0)]

checkSquare :: Board -> Point -> Bool
checkSquare board p 
    | getSquare board p == (Just '@') 
        = (<4) 
        . length 
        . filter (==Just '@') 
        $ getSquare board <$> getNeighbours p -- `debug` ("Crate in " ++ show p)
    | otherwise = False -- `debug` ("No crate in " ++ show p)

nextBoard :: Board -> [Point] -> Board
nextBoard board points = board'
    where
        board' = [[newSquare (x, y) | x <- [0..c-1]] | y <- [0..r-1]]
        (r, c) = (length board, length $ board !! 0)
        newSquare (x, y) = if (x, y) `elem` points then '.' else board !! y !! x

removeBoxes :: (Board, Int) -> (Board, Int)
removeBoxes (board, _) = (board', count) `debug` show count
    where
        liftableBoxes = filter (checkSquare board) . indicesGrid $ board
        count         = length liftableBoxes
        board'        = nextBoard board liftableBoxes

main :: IO ()
main = do
    board <- filter (/="\n") . lines <$> readFile "inputs/Day04.txt"
    -- board <- filter (/="\n") . lines <$> readFile "Day04.test.txt"
    let (rows, cols) = (length board, length $ board !! 0)
    -- let validSlots = map (fromEnum . checkSquare board) $ indicesGrid board
    -- print $ sum validSlots

    -- Part 2
    let result = sum . map snd . takeWhile ((/= 0) . snd) . tail . iterate removeBoxes $ (board, 0)
    print result
