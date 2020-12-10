import Data.List

filtCartProd :: [Int] -> [(Int,Int)]
filtCartProd xs = [(x,y) | x <- xs, y <- xs, x /= y]

sumPairs :: (Int,Int) -> Int
sumPairs (a,b) = a+b

checkValue :: Int -> (Int, Int) -> Bool
checkValue x (a,b)
    | sumPairs (a,b) == x = True
    | otherwise = False

checkSum25 :: [Int]-> Int
checkSum25 (x:xs)
    | not (or c) = r
    | otherwise = checkSum25 xs
    where
        t = take 25 (x:xs)
        (r:rs) = drop 25 (x:xs)
        a = filtCartProd t
        c = map (checkValue r) a

innerRecur :: [Int] -> Int -> Int -> [Int]
innerRecur [] n v = []
innerRecur (x:xs) n v
    | sum t == v = t
    | otherwise = innerRecur xs n v
    where
        t = take n (x:xs)

outerRecur :: [Int] -> Int -> Int -> [Int]
outerRecur (x:xs) n v
    | p == [] = outerRecur (x:xs) (n+1) v
    | otherwise = p
    where
        p = innerRecur (x:xs) n v

main = do
    f <- readFile("input.txt")
    let x = map (read::[Char]->Int) $ lines f
    let s1 = checkSum25 x
    let y = [y | y <- x, y < s1]
    let s2 = outerRecur y 2 s1
    print $ s1
    print $ (minimum s2) + (maximum s2)
    print $ s2
