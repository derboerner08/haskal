import Data.Char

add :: Integer->Integer->Integer

add a b = a + b



and a b = a&&b

sub :: Integer->Integer->Integer

sub a b = a-b

pot :: Integer->Integer->Integer

pot x 0 = 1
pot x y = x*(pot x (y-1))

fak :: Integer->Integer
fak 0 = 1
fak x = x*(fak (x-1))

len :: [a]->Integer
len [] = 0
len (x:xs) = 1 + (len(xs))

hd (x:xs) = x

tl :: [a]-> [a]
tl (x:xs) = xs

cons x y = x:y

isempty y = y==[]

laenge [] = 0
laenge (x:xs) = 1 + laenge xs

maxi
maxi (x:xs) 
 |x >= hd xs = maxi(x:tl xs)
 otherwise = maxi xs


verdoppeln [] = [] 
verdoppeln (x:xs) = x:x:verdoppeln xs

taken:: Int ->[a] -> [a]
taken 0 (x:xs) = []
taken n (x:xs)
 |n>0 = x:take (n-1) xs

drope 0 xs = xs
drope n (x:xs)
 |n>0 = drop (n-1) xs 

letter2num :: Char->Int
letter2num x = ord x - 96

num2letter :: Int->Char
num2letter x = chr (x+96)

caesar :: Int -> [Char] -> [Char]
caesar _ [] = []
caesar a (x:xs) = chr (ord x + a) : caesar a xs