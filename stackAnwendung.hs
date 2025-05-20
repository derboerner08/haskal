import StackLK (Stack, new, isEmpty, push, pop, top)

ausgeglichen :: String -> Bool
ausgeglichen liste = _ausgeglichen new liste

_ausgeglichen :: Num a => StackLK.Stack a -> String -> Bool
_ausgeglichen stack [] |(isEmpty stack) = True
                            |otherwise = False
_ausgeglichen stack (')':b)     |(isEmpty stack) = False
                                    |otherwise = _ausgeglichen (pop stack) b
_ausgeglichen stack ('(':b) = _ausgeglichen (push 0 stack) b
_ausgeglichen stack (a:b) = _ausgeglichen stack b

check' str = isEmpty(_check' str new)
_check' "" stack = stack
_check' (x:xs) stack 
    |x == '(' = _check' xs (push 1 stack)
    |x == ')' && isEmpty stack = push 1 stack --etwas auf den Stack um False zu bekommen :) 
    |x == ')' = _check' xs (pop stack)
    |otherwise = _check' xs stack


-- SIND ALLE KLAMMERN ZU
klm_chk :: String -> Stack Char -> Bool
klm_chk "" Empty = True
klm_chk "" stk = False
klm_chk (f:r) Empty
    | f == ')'  = False
    | f == '('  = klm_chk r (Stack f (Empty))
    | otherwise = klm_chk r Empty
klm_chk (f:r) (Stack el (rs)) 
    | f == ')' && el == '(' = klm_chk r rs
    | f == ')' && el == ')' = False
    | f == '('              = klm_chk r (Stack f ((Stack el (rs))))
    | otherwise             = klm_chk r (Stack el (rs))




{--
--- Aufgabe: Versuchen, die Fallunterscheidung zu fixen
--}
