--This function receives as input one list of any type and 
-- returns the list with the elements on the inverted order. 

invertelst:: [any] -> [any]
invertelst [] = []
invertelst (head:tail) = (invertelst tail)++[head]