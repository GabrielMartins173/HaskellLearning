-- This function receives as input one list of numbers and one integer number
-- and returns the list of numbers with all the elements potentialized using 
-- the integer number as an expoent. 

powlist :: [Float] -> Int -> [Float]
powlist [] expoent = [] -- If we receive an empty list we should return an empty list. 
powlist (head:tail) expoent = [head^expoent] ++ powlist tail expoent


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
