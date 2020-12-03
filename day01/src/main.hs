pair2020 :: [Int] -> Int
checkLists :: ([Int], [Int]) -> Int
checkPair :: (Int, Int) -> Bool

checkPair (a,b) =  a+b == 2020

checkLists (l, m)
    | length m == 1 && not (checkPair (a, b)) = checkLists (x, tail x)
    | checkPair (a, b) = a*b
    | otherwise = checkLists (l, tail m)
    where
        a = head l
        b = head m
        x = tail l

pair2020 l =
    checkPair (l, tail l)

main = do
    f <- readFile "input.txt"
    let c = lines f
        s = map (read::String->Int) c
    print $ pair2020 $ s
