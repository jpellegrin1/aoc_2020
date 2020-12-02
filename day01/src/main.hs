pair2020 :: [Int] -> Int
sumTuple :: (Int, Int) -> Int
multTuple :: (Int, Int) -> Int

sumTuple (a, b) = a + b
multTuple (a, b) = a * b

pair2020 l =
    checkPair l []
    where
        checkPair m [] = pair2020 (tail l)
        checkPair m (x:xs) =
            if sumTuple (head l, x) == 2020
                then multTuple (head l, x)
                else checkPair l xs

main = print $ pair2020 []
