import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec

data Pw = Pw { minI :: Int, maxI :: Int, reqChar :: Char, pw :: [Char]} deriving (Show)

parsePw :: Parsec [Char] () Pw
parsePw = do
    minC <- many1 digit
    char '-'
    maxC <- many1 digit
    space
    reqChar <- letter
    char ':'
    space
    pwString <- many1 letter
    return $ Pw (read minC) (read maxC) reqChar pwString

listPw :: Parsec String () [Pw]
listPw = many $ do
    pw <- parsePw
    newline
    return pw

isValidPw :: Pw ->  Integer
isValidPw inputPw
    | (n <= (maxI inputPw)) && (n >= (minI inputPw)) = 1
    | otherwise = 0
    where
        n = charCount (reqChar inputPw) (pw inputPw) 0

charCount :: Char -> [Char] -> Int -> Int
charCount c [] x = x
charCount c (p:ps) x
    | c == p = charCount c ps (x+1)
    | otherwise = charCount c ps x

isValidPw2 :: Pw ->  Integer
isValidPw2 inputPw
    | ((a || b) && (not (a && b)))  = 1
    | otherwise = 0
    where
        a = (getCharAtPos (pw inputPw) (minI inputPw)) == Just (reqChar inputPw)
        b = (getCharAtPos (pw inputPw) (maxI inputPw)) == Just (reqChar inputPw)

getCharAtPos :: [Char] -> Int ->  Maybe Char
getCharAtPos s x
    | x > length s = Nothing
    | otherwise = Just (s !! (x-1))


dropParseError :: Either ParseError [Pw] -> [Pw]
dropParseError (Left a) = []
dropParseError (Right b) = b

main = do
    f <- readFile "input.txt"
    let passwords = dropParseError $ (parse listPw "" f)
    let s1 = map (isValidPw) passwords
    let s2 = map (isValidPw2) passwords
    print $ sum s1
    print $ sum s2
