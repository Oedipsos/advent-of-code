import Data.List (elemIndex)

findMaxJoltage :: Int -> String -> String
findMaxJoltage 0 bank = ""
findMaxJoltage n bank = m : findMaxJoltage (n-1) bank'
    where
        l = length bank - n + 1
        m = maximum . take l $ bank
        bank' = drop (k+1) bank
        (Just k) = elemIndex m bank

main = do
    -- let content = lines "987654321111111\n811111111111119\n234234234234278\n818181911112111"
    content <- lines <$> readFile "inputs/Day03.txt"
    
    let result1 = sum . map (read . findMaxJoltage 2) $ content
    let result2 = sum . map (read . findMaxJoltage 12) $ content
    
    print (result1, result2)