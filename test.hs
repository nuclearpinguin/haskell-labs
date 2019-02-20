import qualified  Data.List as DL
-- lastButOne xs = if (length xs ) == 2
--     then head xs
--     else lastButOne (tail xs)

lastButOne (x:xs) = if length(xs) == 1
    then x
    else lastButOne xs


-- channelSerie (Channel start end serie) = serie
-- channeLen (Channel start end serie) = length(serie)

-- this is tideier approach:
channelSerie (Channel _ _ serie) = serie
channeLen (Channel _ _  serie) = length(serie)

type Start_tstmp = Int
type End_tstmp = Int
type Serie = [Double]
data Channel = Channel Start_tstmp End_tstmp Serie
    deriving (Show)

    
-- data Color = Red
--     | Orange
--     | Yellow 
--     | Green 
--     | Blue
--     | Indigo 
--     | Violet
--     deriving (Eq, Show)

type CustomerID = Int
type Address = String

data Customer = Customer {
    customerID :: CustomerID , 
    customerName :: String ,
    customerAddress :: Address
} deriving (Show)

-- data Maybe a = Just a
--             | Nothing

--  RECURSIVE TYPES
data List a = Cons a (List a)
            | Nil
            deriving(Show)


-- Exercise 60.1    
fromList (Cons a b) = a : (fromList(b))
fromList (Nil) = []

-- Guards and pattern matching
myDrop n xs | n <= 0 = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

-- Exercise 69.1/2
len :: [a] -> Integer
len (_:xs) = 1 + len(xs)
len [] = 0

-- Exercise 69.3
mean :: [Float] -> Maybe Float
mean [] = Nothing
mean xs = Just( sum xs / fromInteger( len xs ) )

-- Exercise 69.4
rev :: [a] -> [a]
rev (x:xs) = rev (xs) ++ [x]
rev [] = []

palindrome :: [a] -> [a]
palindrome xs = xs ++ rev (xs)

-- Exercise 695
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == rev(xs)

-- Exercise 69.6
compareLen as bs 
    | len(as) < len(bs) = LT
    | len(as) > len(bs) = GT
    | len(as) == len(bs) = EQ

-- sortList :: a -> [a] -> [a]
sortList xs = DL.sortBy compareLen xs

-- Exercise 69.7/8
myIntersperse :: a -> [[a]] -> [a]
myIntersperse sep (x:xs)
    | length(xs) >= 1 = x ++ [sep] ++ myIntersperse sep xs 
    | length(xs) == 0 = x
myIntersperse _ [] = []

-- Exercise 69.10/11
data Point = Point {
    getX::Float ,
    getY:: Float
} deriving (Show)

data Line = Line {
    slope :: Float,
    coef :: Float
} deriving (Show)

line :: Point -> Point -> Line
line p q = let 
    a = (getY q - getY p) / (getX q - getX p)
    b = getY p - a * getX p
    in Line a b

eval :: Float -> Line -> Float
eval x l = (slope l) * x + (coef l) 

data Direction = Lef
    | Rig
    | Hor
    deriving(Show, Eq)

direction :: Point -> Point -> Point -> Direction
direction a b c = let
    l = line a b
    in if (getX a < getX b)
        then positionAB c l
        else positionBA c l

positionAB :: Point -> Line -> Direction
positionAB p l 
    | getY p < eval (getX p) l = Rig
    | getY p > eval (getX p) l = Lef
    | getY p == eval (getX p) l = Hor

positionBA :: Point -> Line -> Direction
positionBA p l 
    | getY p < eval (getX p) l = Lef
    | getY p > eval (getX p) l = Rig
    | getY p == eval (getX p) l = Hor

-- Exercise 69.12
turnResolve :: [Point] -> [Direction]
turnResolve (a:(b:(c:xs))) = let
    ys = [b,c] ++ xs
    in [direction a b c] ++ turnResolve ys
turnResolve _ = []

test = let
    a = Point 0 0
    b = Point 1 1
    c = Point 2 1
    d = Point 3 2
    xs = [a,b,c,d]
    in turnResolve (xs)

-- Exercise 69.13
-- direction could be magicaly calculated:
-- p2.x - p1.x)*(p3.y - p1.y) - (p2.y - p1.y)*(p3.x - p1.x)
ccw a b c = (getX b - getX a)*(getY c - getY a) - (getY b - getY a)*(getX c - getX a)

direction2 a b c 
    | ccw a b c > 0 = Lef
    | ccw a b c < 0 = Rig
    | ccw a b c == 0 = Hor

test2 = let
    a = Point 0 0
    b = Point 1 1
    c = Point 2 1
    d = Point 3 2
    xs = [a,b,c,d]
    in direction2 a b c