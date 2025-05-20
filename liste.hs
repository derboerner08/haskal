
module Liste (new, headLK, lengthLK, lastLK, reverseLK) 
    -- Schnittstelle: Welche Operationen sind implementiert, toBeCompleted!
    -- Zum Beispiel: tail', length', index', reverse', sort'

where
    data Liste a = Empty | K a (Liste a) -- rekursive Definition der Liste und ihres Konstruktors K

    -- Typsignaturen der ADT-Operationen
    new :: Liste a
    headLK :: Liste a -> a
    tailLK :: Liste a -> Liste a
    lengthLK :: Liste a -> Int
    lastLK :: Liste a -> a
    reverseLK :: Liste a -> Liste a
    _reverseLK :: a -> Liste a -> Liste a -> Liste a
    append' :: Liste a -> a -> Liste a 
    sort' :: Ord a => Liste a -> Liste a
    getSmallerEq :: Ord a => Liste a -> a -> Liste a
    getBigger :: Ord a => Liste a -> a -> Liste a
    concat' :: Liste a -> Liste a -> Liste a
    -- toBeCompleted!


    -- die zugehoerigen Definitionen
    new = Empty

    headLK (K a rest) = a -- head (x:xs) = x
    headLK Empty = error "Leere Liste"


    tailLK Empty = Empty
    tailLK (K a b) = b 
    
    lengthLK Empty = 0
    lengthLK (K a b) = 1 + lengthLK b

    lastLK Empty = error "empty list"
    lastLK (K a Empty) = a
    lastLK (K a b) = lastLK b

    reverseLK Empty = Empty
    reverseLK (K a b) = _reverseLK a b new

    _reverseLK a Empty acc = (K a acc)
    _reverseLK a b acc = _reverseLK (headLK b) (tailLK b) (K a acc)


    append' Empty el = (K el new)
    append' (K a b) el = (K a (append' b el))
    
    getSmallerEq Empty _ = Empty
    getSmallerEq (K a b) n 
        | a > n = getSmallerEq b n 
        | otherwise = (K a (getSmallerEq b n))

    getBigger Empty _ = Empty
    getBigger (K a b) n 
        | a <= n = getBigger b n 
        | otherwise = (K a (getBigger b n))
    
    concat' Empty Empty = Empty
    concat' (K a b) Empty = (K a b)
    concat' Empty (K c d) = (K c d)
    concat' a (K c d) = concat' (append' a c) d

    sort' Empty = Empty
    sort' (K a Empty) = (K a Empty)
    sort' (K a b) =  concat' (sort' (getSmallerEq b a)) (K a (sort' (getBigger b a)))
    
    



 

    -- Das Modul ist eine Instanz der Typklasse Show, brauchen wir, um unsere Listen anzeigen zu lassen.
    -- Hier soll nichts geaendert werden!!!
    instance (Show a) => Show (Liste a) where
                        show = showListe -- die show-Funktion der bekannten Datentypen wird ueberschrieben

    showListe Empty   = " "
    showListe (K a b) = show a ++ " " ++ showListe b    -- dafuer wird die bekannte show-Funktion 
                                                        -- der Datentypen (Int, Char, ...) genutzt



    --Testumgebung
    liste1 = K 4 (K 3 (K 2 (K 1 new)))
    liste2 = K 'a' (K 'b' (K 'c' (K 'd' new)))
    --liste3 = insert 4 (insert 2 (insert 6 (insert 5 (insert 1 new))))