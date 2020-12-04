import System.Environment

tobogganRide :: [String] -> (Int, Int) -> Int
checkTree :: Char -> Bool
countTree :: Int -> Int -> (Int, Int) -> Int -> [String] -> Int
multList :: [Int] -> Int

multList [] = 1
multList (x:xs) = x * multList xs

checkTree a = a == '#'

countTree n m (a,b) r [] = n
countTree n m (a,b) r (x:xs) =
    if (checkTree $ (x !! (m `mod` (length x)))) && (r `mod` a) == 0
        then countTree (n+1) (m+b) (a,b) (r+1) xs
        else (if r `mod` a == 0
                then countTree n (m+b) (a,b) (r+1) xs
                else countTree n m (a,b) (r+1) xs
                )

tobogganRide c (a,b) = countTree 0 0 (a,b) 0 c

main = do
    f <- readFile "input.txt"
    let c = lines f
        l = ([tobogganRide c (1,1)
            ,tobogganRide c (1,3)
            ,tobogganRide c (1,5)
            ,tobogganRide c (1,7)
            ,tobogganRide c (2,1)])
    print $ multList l
