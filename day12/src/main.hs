import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec
import Data.Either

parseMove :: Parsec String () (Char,Int)
parseMove = do
    a <- letter
    b <- many1 digit
    newline
    return $ (a,read  b)

parseMoves :: Parsec String () [(Char,Int)]
parseMoves = do
    m <- many (try parseMove)
    return $ m

turnShipS1 :: (Char,Int) -> Int -> Int
turnShipS1 (a,b) d
    | a == 'R' = (d + n) `mod` 4
    | a == 'L' = (d - n) `mod` 4
    where
        n = (b `div` 90)

moveShipS1 :: (Char,Int) -> (Int,Int,Int) -> (Int,Int,Int)
moveShipS1 (a,b) (d,x,y)
    | a == 'N' = (d,x,y+b)
    | a == 'S' = (d,x,y-b)
    | a == 'E' = (d,x+b,y)
    | a == 'W' = (d,x-b,y)
    | a == 'F' && d == 0 = (d,x+b,y)
    | a == 'F' && d == 1 = (d,x,y-b)
    | a == 'F' && d == 2 = (d,x-b,y)
    | a == 'F' && d == 3 = (d,x,y+b)
    | otherwise = (turnShipS1 (a,b) d , x, y)

finalLocationS1 :: [(Char,Int)] -> (Int,Int,Int) -> (Int,Int,Int)
finalLocationS1 (x:[]) c = moveShipS1 x c
finalLocationS1 (x:xs) c = finalLocationS1 xs (moveShipS1 x c)

moveShipS2 :: ((Int, Int), (Int,Int)) -> Int  -> ((Int, Int), (Int,Int))
moveShipS2 ((mx,my),(wx,wy)) b =
    let
        new_mx = mx + b*wx --x_s*b*wx
        new_my = my + b*wy -- y_s*b*wx
    in
        ((new_mx,new_my),(wx, wy))

rotWP :: (Char, Int) -> ((Int, Int), (Int,Int)) -> ((Int, Int), (Int,Int))
rotWP (a,b) ((mx,my),(wx,wy))
    | (a,b) ==  ('L',90) || (a,b) == ('R', 270)  = ((mx,my),(-wy,wx))
    | (a,b) == ('L',180) = ((mx,my),(-wx,-wy))
    | (a,b) == ('R',90) || (a,b) == ('L', 270) = ((mx,my),(wy,-wx))
    | (a,b) == ('R',180) = ((mx,my),(-wx,-wy))
    | otherwise = ((mx,my),(wx,wy))

updatePositions :: (Char, Int) -> ((Int, Int), (Int,Int)) ->  ((Int, Int), (Int,Int))
updatePositions (a,b) ((mx,my),(wx,wy))
    | a == 'N' = ((mx,my), (wx, wy+b))
    | a == 'S' = ((mx,my), (wx, wy-b))
    | a == 'E' = ((mx,my), (wx+b, wy))
    | a == 'W' = ((mx,my), (wx-b, wy))
    | a == 'F' = moveShipS2 ((mx,my),(wx,wy)) b
    | otherwise = rotWP (a,b) ((mx,my),(wx,wy))


finalLocationS2 :: [(Char,Int)] -> ((Int, Int), (Int,Int)) -> ((Int, Int), (Int,Int))
finalLocationS2 (x:[]) c = updatePositions x c
finalLocationS2 (x:xs) c = finalLocationS2 xs (updatePositions x c)

main = do
    f <- readFile("input.txt")
    let c = fromRight [] (parse parseMoves "" f)
    let s1 = finalLocationS1 c (0,0,0)
    let s2 = finalLocationS2 c ((0,0),(10,1))
    print $ s1
    print $ s2
