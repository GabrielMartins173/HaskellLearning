-- This function is used as auxiliar function for the 
-- powlist function, this is only the implementing 
-- of the potentiation operation having any positive float number  
-- as base and one integer number as expoent.  

pow :: Float -> Int -> Float
pow base expoent | (expoent == 0) = 1 -- If the expoent is equal to 0 we should return 1. 
    | otherwise = base ^ expoent

        
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
