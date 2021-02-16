-- This function is an auxiliar that generates an infinite fibonnaci sequence.
fib a b = a:fib b (a+b)


-- This function returns the n first numbers of the fibonacci sequence
-- Where n is an integer number passed by parameter
fibonacci :: Int -> [Int]
fibonacci 0 = [0]
fibonacci 1 = [0,1]
fibonacci n = take n (fib 0 1)

-- main function to test the implemented function. 

main :: IO()
main = do

        putStrLn "5 elements on the fibonacci sequence"
        print(fibonacci 5)
        putStrLn " "
        putStrLn "10 elements on the fibonacci sequence"
        print(fibonacci 10)
        putStrLn " "
        putStrLn "15 element on the fibonacci sequence"
        print(fibonacci 15)
        putStrLn " "



-- This function returns the n first numbers of that are potentiations with base 2
-- Where n is an integer number passed by parameter
twoPot :: Int -> [Int]
twoPot n = take n twoPot where twoPot = 2:map (*2) twoPot

-- This function returns the n first numbers of the sequence defined by:
-- n0 = 2, ni = ni-1². Where n is an integer number passed by parameter
sequenceNumbers :: Int -> [Int]
sequenceNumbers n = take n sequenceNumbers where sequenceNumbers = 2:map (^2) sequenceNumbers

-- This function receives as parameter two integer numbers 'e' and 'n', the first number 
-- will be replicated n times into a list 

replica :: Int -> Int -> [Int] 
replica element repetitions | (repetitions == 0) = []
                            | otherwise = [element] ++ replica element (repetitions - 1) 

-- This function receive as parameter two lists of float numbers with values between
-- Zero and 1 and returns a Float value between Zero and 1 that represents the 
-- similarity of this two lists. 

similarity :: [Float] -> [Float] -> Float
similarity xs ys = (prod_int xs ys) / ((norma xs) * (norma ys))

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