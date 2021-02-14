

-- main function to test the implemented function. 

main :: IO()
main = do

        let list = [0.1, 0.2, 0.3, 0.4]

        -- test for the somatorio function
        putStrLn "Norm for the numbers 0.1, 0.2, 0.3, 0.4 should be 0.5477226"
        print(norma list)
        putStrLn " "
        putStrLn "Norm of an empty list should be Zero "
        print(norma [])
        putStrLn " "








-- This function receives as a parameter one list of float numbers between 
-- zero and 1, and returns the norm of this numbers. 

norma :: [Float] -> Float
norma ns = sqrt (sum (map (^2) ns))


-- This function receives as a parameter a list of numbers and 
-- returns one float number that is the sum of all numbers on this 
-- input list. 

somatorio :: [Float] -> Float
somatorio [] = 0
somatorio (h:t) = h + (foldr (+) 0 t)


-- This function receives as input one list of any type and 
-- returns the list with the elements on the inverted order. 

invertelst :: [any] -> [any]
invertelst [] = [] -- If we receive an empty list we should return an empty list.
invertelst (head:tail) = (invertelst tail)++[head]
     

-- This function is used as auxiliar function for the 
-- powlist function, this is only the implementing 
-- of the potentiation operation having any positive float number  
-- as base and one integer number as expoent.  

pow :: Float -> Int -> Float
pow base expoent | (expoent == 0) = 1 -- If the expoent is equal to 0 we should return 1. 
    | otherwise = base ^ expoent

-- This function receives as input one list of numbers and one integer number
-- and returns the list of numbers with all the elements potentialized using 
-- the integer number as an expoent. 

powlist :: [Float] -> Int -> [Float]
powlist [] expoent = [] -- If we receive an empty list we should return an empty list. 
powlist (head:tail) expoent = [head^expoent] ++ powlist tail expoent