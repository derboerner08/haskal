module Punkt where
    data Punkt= Punkt Float Float

    instance Show Punkt where
            show = showPunkt
    showPunkt (Punkt x y) = "("++show x ++ "|" ++show y ++")"

