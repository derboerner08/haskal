-- Methoden spezifizieren und  implementieren
module Set (Set) -- new, isEmpty, isIn, insert, remove, isIn, cardinality)
where
data Set a = Empty | K a (Set a)

new :: Set a
isEmpty :: Set a -> Bool
isIn :: Eq a => a -> Set a -> Bool
insert :: Eq a => a -> Set a -> Set a
remove :: Eq a => a -> Set a -> Set a
cardinality  :: Set a -> Int

new = Empty

isEmpty Empty = True
isEmpty (K a b) = False

isIn e Empty = False
isIn e (K a rest) = e == a || (isIn e rest)
    
insert e aSet
    | isIn e aSet = aSet
    | otherwise = K e aSet

remove _ Empty = Empty
remove elem (K a b) |(a == elem) = b
                    |otherwise = (K a (remove elem b))


cardinality Empty = 0
cardinality (K _ b) = 1 + cardinality b

instance (Show a) => Show (Set a) where
    show = showSet
showSet aSet = "{" ++ showM aSet ++ "}"
showM Empty  = ""
showM (K a Empty) = show a
showM (K a b) = show a ++ "," ++ showM b


--Testmengen
m1= K 3 Empty 
m2 = K 7 (K 9 (K 4 m1))
m3 = K 5 (K 9 m2) -- ist keine Menge, denn der Konstruktor prueft nicht, ob das Element schon vorhanden
