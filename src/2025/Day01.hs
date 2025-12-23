import Data.Foldable (for_)

solve :: (Int, Int) -> String -> (Int, Int)
solve (start, n) ('L':step) = ( (start - s) `mod` 100, n + turns + (if overshoot then 1 else 0) )
	where
		s = read step
		turns = (if start == 0 then s - 1 else s) `div` 100
		overshoot = start /= 0 && s `mod` 100 > start

solve (start, n) ('R':step) = ( (start + s) `mod` 100, n + turns + (if overshoot then 1 else 0) )
	where
		s = read step
		turns = (if start == 0 then s - 1 else s) `div` 100
		overshoot = s `mod` 100 > 100 - start

main = do
	content <- lines <$> readFile "inputs/Day01.txt"
	let states = scanl solve (50, 0) $ content
	let result1 = length . filter (== 0) . map fst $ states
	print result1

	-- Part 2
	let result2 = snd (last states)
	
	-- for_ (zip ("":content) states) print
	print $ result1 + result2

