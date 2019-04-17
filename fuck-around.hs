module FuckAround (Color, Animal(..)) where

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "You are underweight! You emo, you..."
    | bmi <= 25.0 = "You are normal. I bet you are ugly!"
    | bmi <= 30.0 = "You are fat!!!"
    | otherwise   = "Congratulations! You are a whale!"
    where bmi = weight/height^2

max' :: Ord a => a -> a -> a
max' a b
    | a >= b    = a
    | otherwise = b

compare' :: Ord a => a -> a -> Ordering
compare' a b
    | a < b  = LT
    | a == b = EQ
    | a > b  = GT

cylinder :: RealFloat a => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r^2
    in  sideArea + 2 * topArea

cylinder' :: RealFloat a => a -> a -> a
cylinder' r h =
    sideArea + 2 * topArea
    where 
        sideArea = 2 * pi * r * h
        topArea  = pi * r^2

describeList :: [a] -> String
describeList list = case list of []     -> "Empty"
                                 (x:[]) -> "One element"
                                 xs     -> "More than one element"

data Color = Red | Blue | Yellow deriving (Show, Eq, Ord)
getColor :: [Char] -> Color
getColor string = 
    case string of "red"  -> Red
                   "blue" -> Blue
                   "yellow" -> Yellow

-- show :: Color -> String
-- show color =
--     case color of Red    -> "red"
--                   Blue   -> "blue"
--                   Yellow -> "yellow"

data Animal = Dog {name :: [Char]} | Cat {name :: [Char]} deriving (Show)

data Vector a = Vector a a a deriving (Show, Eq)

vplus :: Num a => Vector a -> Vector a -> Vector a 
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) =  Vector (x1 + x2) (y1 + y2) (z1 + z2)

vscalar :: Num a => Vector a -> Vector a -> a
vscalar (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

data Person = Person { firstName :: String, lastName :: String, age :: Int } deriving (Show, Eq, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Enum)

-- ****************************************
-- Monoid'

class Monoid' a where
    combine' :: a -> a -> a
    zero' :: a

data Account = Account { balance :: Double } deriving (Show)

instance Monoid' Account where
    combine' a b = Account ((balance a) + (balance b))
    zero' = Account 0

fold' :: Monoid' a => [a] -> a
fold' [] = zero'
fold' list = snd (foldAcc list zero')
    where
        foldAcc :: Monoid' a => [a] -> a -> ([a], a)
        foldAcc [] acc     = ([], acc)
        foldAcc (a:as) acc = foldAcc as (combine' a acc)

-- ****************************************
-- Option
data Option a = None | Some a deriving (Show)

map' :: (Option a) -> (a -> b) -> (Option b)
map' optionA f = 
    case optionA of None   -> None
                    Some a -> Some (f a)

instance (Eq a) => Eq (Option a) where
    Some a == Some b = a == b
    None   == None = True
    _ == _ = False

-- *****************************************
class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = True
    yesno _ = False

instance YesNo Integer where
    yesno 0 = True
    yesno _ = False

instance YesNo Double where
    yesno 0 = True
    yesno _ = False

instance YesNo Float where
    yesno 0 = True
    yesno _ = False

instance YesNo [a] where
    yesno [] = True
    yesno _ = False

instance YesNo Bool where
    yesno = id

instance YesNo (Option a) where
    yesno None = True
    yesno _ = False

-- **************************************
-- Functor typeclass
class Functor' f where
    fmap' :: f a -> (a -> b) -> f b

instance Functor' [] where
    fmap' [] _   = []
    fmap' list f = reverse (snd (mapAcc f list []))
        where
            mapAcc :: (a -> b) -> [a] -> [b] -> ([a], [b])
            mapAcc _ [] acc = ([], acc)
            mapAcc func (y:ys) acc = mapAcc func ys ((func y):acc)

instance Functor' Option where
    fmap' None _ = None
    fmap' (Some a) f = Some (f a)

instance Functor' (Either a) where
    fmap' (Left l) _ = Left l
    fmap' (Right r) f = Right (f r)

-- ****************************************
-- higher Order functions
f :: Integral a => a -> a
f num 
    | even num = num `div` 2
    | odd num = num * 3 + 1


chain :: Integral a => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:(chain (n `div` 2))
    | odd n  = n:(chain (n * 3 + 1))

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15
filter (\n -> length (chain n) > 15) [1..100]


map (\n -> length (chain n)) [1..100]

































