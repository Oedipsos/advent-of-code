import Data.Numbers (factors)

splitOn :: [Char] -> String -> [String]
splitOn delimiters s = case dropWhile (`elem` delimiters) s of
	"" -> [] 
	s' -> w : splitOn delimiters s''
		where (w, s'') = break (`elem` delimiters) s'

searchInvalid :: Int -> Int -> [Int]
searchInvalid start end |
	length start /= length end = [ | d <- factors . length $ ]


main = do
	content <- init <$> readFile "inputs/Day02.txt"
	let input = map (splitOn "-") . splitOn "," $ content
	print input
