filtCartProd :: [Int] -> [Int]
filtCartProd xs = [x*y | x <- xs, y <- xs, x /= y, x+y == 2020, x < y]

filtCartProd2 :: [Int] -> [Int]
filtCartProd2 xs = [x*y*z | x <- xs, y <- xs, z <- xs, x /= y, x/= z, y/=z, x+y+z == 2020, x < y, y <z]

main = do
    f <- readFile("input.txt")
    let x = map (read::[Char]->Int) $ lines f
    let s1 = filtCartProd x
    let s2 = filtCartProd2 x
    print $ s1
    print $ s2
