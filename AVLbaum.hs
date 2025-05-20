module AVL (new, root, inorder, height, insert)

where
    data AVL a = Empty | K (AVL a) a (AVL a)

    new::AVL a
    root :: AVL a -> a
    inorder :: AVL a -> [a]
    height :: AVL a -> Int
    insert :: Ord a => a -> AVL a -> AVL a
    balanceFactor :: AVL a -> Int
    lrot :: AVL a -> AVL a
    rrot :: AVL a -> AVL a
    lrrot :: AVL a -> AVL a
    rlrot :: AVL a -> AVL a
    insertlist :: Ord a => [a] -> AVL a -> AVL a



    new = Empty


    root Empty = error ("keine Wurzel")
    root (K l w r) = w


    inorder Empty = []
    inorder (K l w r) = (inorder l) ++ [w] ++ (inorder r)


    height Empty = 0 
    height (K l w r) = max (1 + (height l)) (1 + (height r))


    insert e Empty = (K Empty e Empty) 
    insert e (K l w r) 
     |e > w = (K l w (insert e r))
     |e < w = (K (insert e l) w r)
     |e == w = (K l w r)


    insertlist [] AVL = AVL
    insertlist (x:s) AVL = insertlist s AVL


    lrot (K l w (K rl rw rr)) = (K (K l w rl) rw rr)


    rrot (K (K ll rw lr) w r) = (K ll rw (K lr w r))


    lrrot (K l w r) = rrot (K (lrot l) w r)


    rlrot (K l w r) = lrot (K l w (rrot r))


    balanceFactor Empty = 0
    balanceFactor (K l w r) = (height l) - (height r)



    -- Das Modul ist eine Instanz der Typklasse Show, brauchen wir, um unsere Listen anzeigen zu lassen.
    -- Hier soll nichts geaendert werden!!!
    instance (Show a) => Show (AVL a) where
                        show = display -- die show-Funktion der bekannten Datentypen wird ueberschrieben

    display :: Show a => AVL a -> [Char]
    display Empty = "[]"
    display t = unlines (fst' (display' t))

    fst' :: (a, b, b, b) -> a
    fst' (a,b,c,d) = a

    display' :: (Show a) => AVL a -> ([String], Int, Int, Int)
    display' (K Empty w Empty) = ([line], length line, 1, length line `div` 2) where line = show w
    display' (K l w Empty) = (firstLine 'l' x n u s : secondLine 'l' u x n  : shiftedLines 'l' lines u, n+u, p+2,n+u `div` 2)
        where (lines,n,p,x) =  display' l
              s = show w
              u = length s

    display' (K Empty w r) = (firstLine 'r' x n u s : secondLine 'r' u x n  : shiftedLines 'r' lines u, n+u, p+2,u `div` 2)
        where (lines,n,p,x) =  display' r
              s = show w
              u = length s

    display' (K l w r) = (firstLine2 x y n m s : secondLine2 u x y n m  : zipLines linesL linesR u p q n m, n+m+u, 2 + max p q , n + (u `div` 2))
        where (linesL,n,p,x) =  display' l
              (linesR,m,q,y) =  display' r
              s = show w
              u = length s

    firstLine :: Char -> Int -> Int-> Int -> [Char] -> [Char]
    firstLine 'r' x n u s = s ++ replicate x '_' ++ replicate (n-x) ' '
    firstLine 'l' x n u s = replicate (x+1) ' ' ++ replicate (n-x-1) '_' ++ s
    secondLine :: Char -> Int -> Int -> Int -> [Char]
    secondLine 'r' u x n = replicate (u+x) ' ' ++ "\\" ++ replicate (n-x-1) ' '
    secondLine 'l' u x n = replicate x ' ' ++ "/" ++ replicate (n-x-1+u) ' '
    shiftedLines :: Char -> [[Char]] -> Int -> [[Char]]
    shiftedLines lr lines u = merge lr lines (replicate u ' ')
    firstLine2 :: Int -> Int -> Int -> Int -> [Char] -> [Char]
    firstLine2 x y n m s = replicate (x+1) ' ' ++ replicate (n-x-1) '_' ++ s ++replicate y '_' ++ replicate (m-y) ' '
    secondLine2 :: Int -> Int -> Int -> Int -> Int -> [Char]
    secondLine2 u x y n m = replicate x ' ' ++ "/" ++ replicate (n-x-1+u+y) ' ' ++ "\\" ++ replicate (m-y-1) ' '

    zipLines :: [[Char]] -> [[Char]] -> Int -> Int -> Int -> Int -> Int -> [[Char]]
    zipLines linesL linesR u p q n m
        | p < q  =  merge2 (linesL++ replicate (q-p) (replicate n ' ')) (replicate u ' ')  linesR
        | q < p =   merge2 linesL (replicate u ' ') (linesR ++ replicate (p-q) (replicate m ' '))
        |otherwise = merge2 linesL (replicate u ' ')  linesR

    merge::  Char -> [[Char]] -> [Char] -> [[Char]]
    merge2:: [[Char]] -> [Char] -> [[Char]]-> [[Char]]
    merge 'l' xs ys = map (++ ys) xs
    merge 'r' xs ys = map (ys ++) xs
    merge2 [] zw [] = []
    merge2 (x:xs) zw (y:ys) = (x++zw++y): merge2 xs zw ys


    baum1 = (K (K new 3 new) 42 (K new 100 new))
