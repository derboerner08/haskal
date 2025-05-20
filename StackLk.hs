module StackLK ( Stack, new, isEmpty, push, pop, top)
 
where
    data Stack a = Empty | S a (Stack a) 
 
 
    new = Empty
 
    isEmpty Empty = True
    isEmpty _ = False
 
    --push el Empty  = (S el new)
    push el stack  = S el stack
 
    pop Empty = error "leerer Stack"
    pop (S a b) = b
 
    top Empty = error "leerer Stack"
    top (S a b) = a
 
   
    instance (Show a) => Show (Stack a) where
                        show = showStack 
 
    showStack Empty   = " "
    showStack (S a b) = show a ++ " " ++ showStack b
 
 
 
    stack1 = S 4 (S 3 (S 2 (S 1 new)))
    stack2 = S 4 (S 3 (S 2 (S 1 new)))
    stack3 = S 'a' (S 'b' (S 'c' (S 'd' new)))