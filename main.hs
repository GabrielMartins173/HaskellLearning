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



-- main function to test the implemented function. 

main :: IO()
main = do

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

        -- individual testing for the pow function
        putStrLn "Operation to check Pow 125^4 should return 1953125.0 (Integer as base)"
        print(pow 125 3)
        putStrLn " "
        putStrLn "Operation to check Pow 10.5^3 should return 1157.625 (Float as base)"
        print(pow 10.5 3)
        putStrLn " "   
        putStrLn "Base scenario where we receive an expoent equals to 0 should return 1.0 (Integer as base)"
        print(pow 90 0)
        putStrLn " "    
        putStrLn "Base scenario where we receive an expoent equals to 0 should return 1.0 (Float as base)"
        print(pow 17.58 0)
        putStrLn " "   

        -- test for the powlist funcition
        putStrLn "Potentialize a List of Integer numbers: "
        print(powlist [1,2,3,4,5,6,7,8,9,10] 2)
        putStrLn " "
        putStrLn "Potentialize a List of Float numbers: "
        print(powlist [1.5,2.3,3.4,4.6,5.4,6.789,7.14,8.364,9.214,10.164654] 3)
        putStrLn " "
        putStrLn "Having 0 as an expoent we should return a list of ones: "
        print(powlist [1.5,2.3,3.4,4.6,5.4,6.789,7.14,8.364,9.214,10.164654] 0)
        putStrLn " "
        putStrLn "Base scenario where we receive an empty list should return an empty list "
        print(powlist [] 2 :: [Float])
        putStrLn " "


