import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec
import Data.List
import Data.Function (on)
import qualified Data.Map as M
import Control.Arrow

parseBP :: Parsec [Char] () ([Char],[Char])
parseBP = do
    fb <- many (char 'F' <|> char 'B')
    rl <- many (char 'R' <|> char 'L')
    return (fb, rl)

listBP :: Parsec String () [([Char], [Char])]
listBP = many $ do
    bp <- parseBP
    newline
    return bp

binarySearch :: [Char] -> (Int, Int) -> (Char, Char) -> Int
binarySearch [] (a,b) (l,u) = a
binarySearch (c:cs) (a,b) (l,u)
    | c == l && (cs == []) = binarySearch [] (a, a) (l,u)
    | c == l = binarySearch cs (a, a + r - 1) (l,u)
    | c == u && (cs == []) = binarySearch [] (b, b) (l,u)
    | c == u = binarySearch cs (a + r, b) (l,u)
    | otherwise = binarySearch cs (a, b) (l,u)
    where
        d = b - a
        r = ((d + 1) `div` 2)

dropParseError :: Either ParseError [([Char],[Char])] -> [([Char],[Char])]
dropParseError (Left a) = []
dropParseError (Right b) = b


parseS1 :: (Int, Int) -> Int
parseS1 (x,y) = (x * 8) + y

parseSeats :: ([Char], [Char]) -> (Int, Int)
parseSeats (r,s) = (binarySearch r (0, 127) ('F', 'B'), binarySearch s (0,7) ('L', 'R'))

sortRows :: Ord a => [(a, b)] -> [(a, b)]
sortRows = sortBy (compare `on` fst)

findMissing :: [Int] -> Int
findMissing [] = -1
findMissing (x:xs)
    | x + 1 /= head xs = x + 1
    | otherwise = findMissing xs


project :: (Int, Int) -> Int
project (a,b) = a

aggList :: [Int] -> Int -> Int
aggList [] c = -1
aggList (x:xs) c
    | (c == 6) && (x /= head xs) = x
    | x == head xs = aggList xs (c+1)
    | otherwise = aggList xs 0

main = do
    f <- readFile "input.txt"
    let c = lines f
    let bp = dropParseError $ (parse listBP "" f)
    let seats = sortRows $ map (parseSeats) bp
    let s1 = maximum $ map (parseS1) seats
    let a = map project seats
    let r = aggList a 0
    let s = [y | (x,y) <- seats, x == r]
    let s2 = findMissing s
    print $ s1
    print $ parseS1 (r, s2)
