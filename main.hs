-- This function receives as a parameter a list of numbers and 
-- returns one float number that is the sum of all numbers on this 
-- input list. 

somatorio :: [float] -> float
somatorio :: (float a) => [a] -> a  
somatorio xs = foldl (\acc x -> acc + x) 0 xs 

-- main function to test the implemented function. 

main :: IO()
main = do

        -- test for the somatorio function
        putStrLn "Sum the numbers on the array of integers: "
        print(somatorio [1,2,3,4,5,6,7,8,9,10])
        putStrLn " "
        putStrLn "Sum the numbers on the array of floats: "
        print(somatorio [1.5,2.3,3.4,4.6,5.4,6.789,7.14,8.364,9.214,10.164654])
        putStrLn " "
