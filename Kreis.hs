module Kreis where 
    import Punkt 
    data Kreis= Kreis Punkt Float

    instance Show Kreis where
        show = showKreis
    showKreis (Kreis a b) = showPunkt a ++ show b




 
