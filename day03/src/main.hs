tobogganRide :: [String] -> Int
checkTree :: Char -> Bool
countTree :: Int -> Int -> [String] -> Int

checkTree a = a == '#'

countTree n m [] = n
countTree n m (x:xs) =
    if checkTree $ x !! (m `mod` 31)
        then countTree (n+1) (m+3) xs
        else countTree n (m+3) xs

tobogganRide c = countTree 0 0 c

main = do
    f <- readFile "input.txt"
    let c = lines f
    print $ tobogganRide $ c
