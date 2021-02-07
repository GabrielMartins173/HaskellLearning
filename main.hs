-- This function receives as input one list of any type and 
-- returns the list with the elements on the inverted order. 

invertelst :: [any] -> [any]
invertelst [] = []
invertelst (head:tail) = (invertelst tail)++[head]


-- This function is used as auxiliar function for the 
-- powlist function, this is only the implementing 
-- of the potentiation operation having any positive float number  
-- as base and one integer number as expoent.  

pow :: Float -> Int -> Float
pow base expoent | (expoent == 0) = 1
    | otherwise = base ^ expoent


-- This function receives as input one list of numbers and one integer number
-- and returns the list of numbers with all the elements potentialized using 
-- the integer number as an expoent. 





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

        -- individual testing for the pow function
        putStrLn "Operation to check Pow 125^4 should return 1953125.0 (Integer as base)"
        print(pow 125 3)
        putStrLn " "
        putStrLn "Operation to check Pow 10.5^3 should return 1157.625 (Float as base)"
        print(pow 10.5 3)
        putStrLn " "       

        -- test for the powlist funcition
        --putStrLn "Potentialize a List of Integer numbers: "
        --print(invertelst [1,2,3,4,5,6,7,8,9,10])
        --putStrLn " "
        --putStrLn "Potentialize a List of Float numbers: "
        --print(invertelst [1.5,2.3,3.4,4.6,5.4,6.789,7.14,8.364,9.214,10.164654])


