-- This function receives as input one list of any type and 
-- returns the list with the elements on the inverted order. 

invertelst :: [any] -> [any]
invertelst [] = [] -- If we receive an empty list we should return an empty list.
invertelst (head:tail) = (invertelst tail)++[head]

-- test for the invertelst function
putStrLn "Invert a List of Integer numbers: "
print(invertelst [1,2,3,4,5,6,7,8,9,10])
putStrLn " "
putStrLn "Invert a List of Float numbers: "
print(invertelst [1.5,2.3,3.4,4.6,5.4,6.789,7.14,8.364,9.214,10.164654])
putStrLn " "
putStrLn "Invert a List of Strings: "
print(invertelst ["list.","inverted ","an ","am ","I ","Hello! "])
putStrLn " "
putStrLn "Base scenario where we receive an empty list should return an empty list "
print(invertelst [] :: [Int])
putStrLn " "

       