import Data.Char

isSucc :: Integer -> Integer -> Bool
isSucc x y = y == x + 1

ifelse :: Integer -> Integer -> Integer
ifelse x y = if x/=0 then x else y

isWeekendDay :: String -> Bool
isWeekendDay x = if x=="Saturday"||x=="Sunday" then True else False

circleArea :: Double -> Double
circleArea x = x * pi

isRightTriangle' :: Integer -> Integer -> Integer -> Bool
isRightTriangle' x y z = if x >= y && x >= z then pyt y z x else if y >= z then pyt x z y else pyt x y z where pyt a b c = a ^ 2 + b ^ 2 == c ^ 2

max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z  = max x (max y z)

mid :: Integer -> Integer -> Integer -> Integer
mid x y z = if x >= min z y && x <= max y z then x else if y >= min z x && y <= max y z then y else z

tell :: Integer -> String
tell x = if x > 2 && mod x 2==0 then "even" else "nic"

isSmallVowel :: Char -> Bool
isSmallVowel x = if (x == 'a' || x == 'e') then True else False

parallelToAxis :: (Integer, Integer) -> (Integer, Integer) -> Bool
parallelToAxis (x, y) (z, n) = if x == z || y == n then True else False

isDivisbleBy :: Integral a => a -> a -> Bool
isDivisbleBy x y = if (mod) x y == 0 then True else False

makeEven :: Integer -> Integer
makeEven x = if mod x 2==0 then x else x * 2

divisible :: Integer -> Integer -> Bool
divisible x y = if y*((div) x y) == x then True else False

strToBool :: String -> Bool
strToBool "ano" = True
strToBool "yes" = True
strToBool "true" = True
strToBool _ = False

hello :: [Char]
hello = "hello, world"

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt((fst p1 - snd p1)^2 + (fst p2 - snd p2)^2)

doubleFact :: Integer -> Integer
doubleFact 0 = 1 
doubleFact 1 = 1 
doubleFact n = n * doubleFact (n - 2)






isRightTriangle :: Integer -> Integer -> Integer -> Bool
isRightTriangle x y z |x<=0||y<=0||z<=0 = error "side must be > 0"
   |x >= y && x >= z = y ^ 2 + z ^ 2 == x ^ 2
   |y >= z = x ^ 2 + z ^ 2 == y ^ 2
   |otherwise = x ^ 2 + y ^ 2 == z ^ 2



logicalAnd :: Bool -> Bool -> Bool
logicalAnd x y = if x then y else False

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y z = x : y : z 

fibonaci :: Integer -> Integer
fibonaci 0 = 0
fibonaci 1 = 1
fibonaci n = fibonaci(n-1)+fibonaci(n-2)


myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:x) = x

neck :: [a] -> a
neck xs = (xs)!!1

isEven :: Integer -> Bool
isEven 0 = True
isEven 1 = False
isEven x = isEven(x-2)

mod3 :: Integral i => i -> i
mod3 0 = 0
mod3 1 = 1
mod3 2 = 2
mod3 x = mod3(x-3)


div3 :: Integral i => i -> i
div3 0 = 0
div3 1 = 0
div3 2 = 0
div3 x = 1+mod3(x-3)

power :: (Num n, Integral i) => n -> i -> n
power _ 0 = 1
power x n = x*power x (n-1)

isPower2 :: Integral i => i -> Bool
isPower2 0 = False
isPower2 1 = True
isPower2 x = even x && isPower2 (div x 2)

firstOrSecond :: Bool -> [a] -> a
firstOrSecond True (x:_) = x
firstOrSecond False (_:x:_) = x

headDef :: a -> [a] -> a
headDef x [] = x
headDef _ (x:_) = x


fibonacci :: Integer -> Integer
fibonacci n | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
   | n < 0     = fibonacci (n + 2) - fibonacci (n + 1)
   | otherwise = n


isOdd :: Integer -> Bool
isOdd n |n == 1 = True
   |n == 0 = False
   |n < 0 = isOdd(-n)
   |n > 1 = isOdd(n-2)


isOdd2 :: Integer -> Bool
isOdd2 1 = True
isOdd2 0 = False
isOdd2 x = if x < 0 then isOdd (-x) else isOdd2 (x - 2)


fibonacci' :: Integer -> Integer
fibonacci' n = helper 0 1 n 

helper :: Integer -> Integer -> Integer -> Integer
helper first second 0 = first
helper first second n | n > 0 = helper (first + second) first (n - 1)
   | n < 0 = helper second (first - second) (n + 1)
   | otherwise = first



kolik2 :: (Integral a, Integral b) => a -> b
kolik2 x = y where (y,_) = f_aux (0,x)

f_aux (cnt, rem) = if (rem<2) || ((mod)rem 2 /= 0) then (cnt,rem) else f_aux (cnt+1, (div) rem 2)


kolik2' i = if (mod i 2 == 0) then kolik2 (div i 2) + 1 else 0


evalF :: Bool -> Bool -> Bool -> Bool -> Bool
evalF True True _ _ = True
evalF False False False _ = True
evalF True False False _ = True
evalF True False True True = True
evalF _ _ _ _ = False


isIsoscelesTriangle :: (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> (Integer, Integer, Integer) -> Bool
isIsoscelesTriangle (a0, b0, c0) (a1, b1, c1) (a2, b2, c2) = let
   d1 = (a0-a1)^2+(b0-b1)^2+(c0-c1)^2
   d2 = (a1-a2)^2+(b1-b2)^2+(c1-c2)^2
   d3 = (a2-a0)^2+(b2-b0)^2+(c2-c0)^2
   in if (d1==d2)||(d2==d3)||(d3==d1) then True else False



   


privip :: Int -> Int -> Int -> Int -> String
privip 10 _ _ _ = "private/8"
privip 192 168 _ _ = "private/16"
privip 127 _ _ _ = "loopback"
privip 172 x _ _ = if x<32&&x>15 then "private/12" else "public"
privip _ _ _ _ = "public"



oddify :: Integral a => [a] -> [a]
oddify [] = []
oddify xs = map (\x -> if odd x then x else x + 1) xs

inputWithOddified :: Integral a => [a] -> [(a, a)]
inputWithOddified xs = zip xs (oddify xs)

nTimes:: a -> Int -> [a]
nTimes x n = nTimes' x n []
nTimes' :: a -> Int -> [a] -> [a]
nTimes' x n acc | n == 0 = acc
                | otherwise = nTimes' x (n-1) (x : acc)


filterShorter :: [String] -> Int -> [String]
filterShorter xs x = filter f xs where f str = length str > x


getNames :: [(String, Integer)] -> [String]
getNames xs = map fst xs

successfulRecords :: [(String, Integer)] -> [(String, Integer)]
successfulRecords xs = filter f xs where f fs = snd fs >= 10

successfulNames :: [(String, Integer)] -> [String]
successfulNames xs = getNames (successfulRecords xs)



toUpperStr :: String -> String
toUpperStr x = map toUpper x


isvowel :: Char -> Bool
isvowel c = elem (toUpper c) "AEIOUY"
vowels :: [String] -> [String]
vowels s = map (filter isvowel) s

assignPrizes :: [String] -> [Integer] -> [(String, Integer)]
assignPrizes x s = zip x s

formatPrizeText :: String -> Integer -> String
formatPrizeText n p = n ++ ": " ++ show p ++ " Kc"

prizeTexts :: [String] -> [Integer] -> [String]
prizeTexts ns ps = zipWith formatPrizeText ns ps


blueLess :: [(Int, Int, Int)] -> [(Int, Int, Int)]
blueLess colors = filter (\(r, g, b) -> b == 0) colors

polychromatic :: [(Int, Int, Int)] -> [(Int, Int, Int)]
polychromatic colors = filter (\(r, g, b) -> (r>0&&g>0)||(g>0&&b>0)||(b>0&&r>0)) colors

greyscale :: [(Int, Int, Int)] -> [(Int, Int, Int)]
greyscale colors = filter (\(r, g, b) -> (r==g)&&g == b) colors

colorsToString :: [(Int, Int, Int)] -> [String]
colorsToString colors = map (\(r, g, b) -> "r:" ++ show r ++ " g:" ++ show g ++ " b:" ++ show b) colors


powerSum :: Num a => [a] -> a
powerSum xs = sum $ map (^2) xs


selectBigger :: Ord a => [(a, a)] -> [(a, a)]
selectBigger xs = filter f xs where f fs = fst fs > snd fs

--foo :: Num a => a -> a -> a
--foo x y = 42-x * (y^2)


digitSum:: Integer -> Integer -> Integer
digitSum base = f 0
  where
    f a 0 = a
    f a n = f (a + r) q
      where
        (q, r) = n `quotRem` base


evenSum :: Integral a => [a] -> a
evenSum = go 0
   where
   go acc [] = acc
   go acc (x:xs)
    | even x = go (acc + x) xs
    | otherwise = go acc xs


trianglePerims :: (Ord a, Num a) => [a] -> [a] -> [a] -> [a]
trianglePerims _ _ [] = []
trianglePerims [] _ _ = []
trianglePerims _ [] _ = []
trianglePerims (x:xs) (y:ys) (z:zs) | x == y && x == z && x > 0 = (x+y+z):(trianglePerims xs ys zs)
   | 2*(maximum[x,y,z])<(x+y+z) && x>0 && y>0 && z>0 = (x+y+z):(trianglePerims xs ys zs)
   | otherwise = trianglePerims xs ys zs




encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode (a:as) = run 1 a as
    where run n x [] = [(n,x)]
          run n x (x2:xss) | x == x2 = run (n+1) x xss
                           | otherwise = (n,x) : run 1 x2 xss


decode :: [(Int, a)] -> [a]
decode = concatMap $ uncurry replicate

rle :: Eq a => [(Int,a)] -> [(Int,a)]
rle = encode.decode


data Colour = Red | Blue | White | Yellow deriving (Eq)
data Direction = Horizontal | Vertical deriving (Eq)
data Flag = Stripes Direction [Colour]
   | Cross Colour Colour Colour
   | Wedge Colour [Colour]
   | Weird deriving (Eq)

czech, austria, norway, slovakia, russia :: Flag
czech = Wedge Blue [White, Red]
austria = Stripes Horizontal [Red, White, Red]
russia = Stripes Horizontal [White, Blue, Red]
norway = Cross Red White Blue
slovakia = Weird 



splitToIncreasing :: Ord a => [a] -> [[a]]
splitToIncreasing [] = []
splitToIncreasing xs = foldr f [] xs
  where
    f a []  = [[a]]
    f a xs'@(y:ys) | a < head y = (a:y):ys
                   | otherwise = [a]:xs'


squish :: Maybe (Maybe a) -> Maybe a
squish (Just (Just a)) = Just a
squish (Just (Nothing)) = Nothing
squish Nothing = Nothing

data Triangle = Plain Integer Integer Integer
              | Isosceles Integer Integer
              | Equilateral Integer
              deriving (Eq, Show)
circumference :: Triangle -> Integer
circumference (Plain a b c) = a + b + c
circumference (Isosceles a b) = a + 2 * b
circumference (Equilateral a) = 3 * a



decapitate :: [[a]] -> [a]
decapitate xs = [x | (x:_) <- xs]


powersOf :: Integral a => a -> [a]
powersOf n = iterate (n*) n
   
unzipWith :: (a -> (b, c)) -> [a] -> ([b], [c])
unzipWith f xs = unzip(map(f) xs)


foo :: Integral a => [a] -> [a]
foo xs = [x*x | x <- xs, x `mod` 3 == 0]