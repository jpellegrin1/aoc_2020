import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec
import Data.Either

data PassportField = PassportField {
fieldName :: [Char]
,fieldValue :: [Char]
} deriving (Read, Show)

data Passport = Passport {
fields :: [PassportField]
} deriving (Read, Show)

parsePassportField' :: Parsec [Char] () PassportField
parsePassportField' = do
    name <- many1 letter
    char ':'
    value <- manyTill anyChar (try (space <|> newline))
    return $ PassportField {fieldName = name, fieldValue = value}

parsePassport' :: Parsec [Char] () Passport
parsePassport' = do
    pf <- manyTill (try parsePassportField') (try newline)
    return $ Passport {fields = pf}

parsePassports' :: Parsec [Char] () [Passport]
parsePassports' = do
    pf <- many (try parsePassport')
    return $ pf

dropParseError :: Either ParseError [Passport] -> [Passport]
dropParseError (Left a) = []
dropParseError (Right b) = b

hasTotalFields :: Passport -> Int
hasTotalFields p
    | length c == 8 = 1
    | length c == 7 && (not d) = 1
    | otherwise = 0
    where
        c = map (fieldName) (fields p)
        d = "cid" `elem` c

readField :: Passport -> String -> String
readField p s = head $ [fieldValue a | a <- (fields p), (fieldName a) == s]

parseHcl :: Parsec String () String
parseHcl = do
    a <- char '#'
    b <- many1 alphaNum
    return $  (a:b)

parseHgt :: Parsec String () (Int,String)
parseHgt = do
    a <- many1 digit
    b <- count 2 letter
    return $  (read a,b)

numValidFields :: Passport -> Int
numValidFields p =
        let
            byr = (read::String->Int) $ readField p "byr"
            byr' = if byr >= 1920 && byr <= 2002 then 1 else 0

            iyr = (read::String->Int) $ readField p "iyr"
            iyr' = if iyr >= 2010 && iyr <= 2020 then 1 else 0

            eyr = (read::String->Int) $ readField p "eyr"
            eyr' = if eyr >= 2020 && eyr <= 2030 then 1 else 0

            hgt = readField p "hgt"
            hgt_pair = fromRight (0,"in") $ (parse parseHgt "" hgt)
            hgt_d = snd hgt_pair
            hgt_v = fst hgt_pair
            hgt' = if (hgt_d == "in") && (hgt_v <= 76) && (hgt_v >= 59) then 1
                else if (hgt_d == "cm") && (hgt_v <= 193) && (hgt_v >= 150) then 1 else 0

            hcl = readField p "hcl"
            hcl' = if isRight (parse parseHcl  "" hcl) then 1 else 0

            ecl = readField p "ecl"
            ecl_list = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
            ecl' = if ecl `elem` ecl_list then 1 else 0

            pid = readField p "pid"
            pid' = if length pid == 9 then 1 else 0
        in
            byr' + iyr' + eyr' + hgt' + hcl' + ecl' + pid'


hasValidPassport :: Passport -> Int
hasValidPassport p
    | (hasTotalFields p == 1)  && (numValidFields p == 7) = 1
    | otherwise = 0

main = do
    f <- readFile ("input.txt")
    let w = fromRight [] $ (parse parsePassports' "" f)
    let v = map (hasTotalFields) w
    let q = map (hasValidPassport) w
    print $ sum v
    print $ sum q
