import Data.Char
import Data.List

add :: Integer->Integer->Integer

add a b = a + b



and a b = a&&b

sub :: Integer->Integer->Integer

sub a b = a-b

pot :: Integer->Integer->Integer

pot x 0 = 1
pot x y = x*(pot x (y-1))

prod :: Integer->Integer->Integer
prod a b= a * b


prod3 a b c= a * b * c

fak :: Integer->Integer
fak 0 = 1
fak x = x*(fak (x-1))

len :: [a]->Integer
len [] = 0
len (x:xs) = 1 + (len(xs))

dub :: (Ord a, Fractional a) => a -> a
dub a
 |abs a>1 = a * 2
  |abs a <= 1 = a*0.5

dub3 a b c= dub (prod3 a b c)

modu a b
    | a >= b = modu (a-b) b
    | otherwise = a

hd :: [a] -> a
hd (x:xs) = x

tl :: [a]-> [a]
tl (x:xs) = xs

cons x y = x:y

isempty y = y==[]

laenge::[Int] -> Int
laenge [] = 0
laenge (x:xs) = 1 + laenge xs

maxi2::[Int] -> Int
maxi2 [] = error "leere Liste"
maxi2 (x:xs)    |xs == [] = x
                |(maxi2 xs)>x = maxi2 xs
                |otherwise = x


verdoppeln :: [a] -> [a]
verdoppeln [] = [] 
verdoppeln (x:xs) = x:(x:verdoppeln xs)

lkTake:: Int -> [Int] -> [Int]
lkTake 0 _ = []
lkTake _ [] = []
lkTake n (x:xs) = x:(lkTake (n-1) xs )

lkDrop 0 b = b
lkDrop a [] = []
lkDrop a (x:xs) = lkDrop (a-1) xs

letter2num :: Char->Int
letter2num x = ord x - 96

num2letter :: Int->Char
num2letter x = chr (x+96)

caesar :: Int -> [Char] -> [Char]
caesar _ [] = []
caesar a (x:xs) = chr (ord x + a) : caesar a xs


istGeordnet::Ord a => [a] -> Bool
istGeordnet [] = True
istGeordnet (x:xs)      |xs == [] = True
istGeordnet (x:y:xs)    |(x <= y) && (istGeordnet (y:xs)) = True
                        |otherwise = False

groupAll x = group(sort x)

verschBuchst wort = length (groupAll wort)

leerzeichenwo a = elemIndices ' ' a

firstWord a = takeWhile (/= ' ') a



