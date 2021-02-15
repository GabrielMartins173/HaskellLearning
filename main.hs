-- This function receive as parameter two lists of float numbers with values between
-- Zero and 1 and returns a Float value between Zero and 1 that represents the 
-- similarity of this two lists. 

similarity :: [Float] -> [Float] -> Float
similarity xs ys = (prod_int xs ys) / ((norma xs) * (norma ys))
-- main function to test the implemented function. 

main :: IO()
main = do

        let list1 = [0.1, 0.4, 0.9]
        let list2 = [0.2, 0.4, 0.5]

        -- test for the somatorio function
        putStrLn "Similarity between the vectors v1 = [0.1, 0.4, 0.9] and v2 = [0.2, 0.4, 0.5] "
        print(similarity list1 list2)
        putStrLn " "
        putStrLn "Similarity using the same vector should be 1 "
        print(similarity list2 list2)
        putStrLn " "


-- This function receives two lists as input and transform this in just one list
-- as output

concatenateList :: [Float] -> [Float] -> [(Float, Float)]
concatenateList [] _ = []
concatenateList (x:xs) (y:ys) = (x, y) : concatenateList xs ys

-- This function multiply the elements inside the tuples formed by the concatenateList
-- function that we built before. 

mutiplyTuples :: [(Float, Float)] -> [Float]
mutiplyTuples xs = [ x * y | (x,y) <- xs]

-- This function receives as parameter two lists with float numbers between zero 
-- and 1 and returns the internal product of this two vectors of numbers. 

prod_int :: [Float] -> [Float] -> Float
prod_int xs [] = 0 -- If any of the lists are empty, the internal product
prod_int [] ys = 0 -- will be zero 
prod_int xs ys = sum(mutiplyTuples(concatenateList xs ys)) 


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