applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15  

numLongChains' :: (Integral a ) => a -> Int  
numLongChains' x = length (filter isLong (map chain [1..x]))  
    where isLong xs = length xs > 15  

divideByX :: (Floating a) => a -> (a -> a)
divideByX x = (/x)

--I wanted to write the commented out signiture, but it didn't work
--lengthGt :: (integral a) => a -> ([b] -> Bool)
lengthGt :: Int -> ([b] -> Bool)
lengthGt x = lenfun
    where lenfun xs = length xs > x

numLongChains'' :: Int -> Int -> Int  
numLongChains'' x y = length (filter (lengthGt y) (map chain [1..x] ))  

reverseM :: [a] -> [a]
reverseM = foldl (\acc x -> x : acc) []

productM :: (Num a ) => [a] -> a
--productM = foldl (\acc x -> acc * x) 1
productM = foldl (*) 1

-- last

filterM :: (a -> Bool) -> [a] -> [a]
filterM p = foldr(\x acc -> if p x then x : acc else acc) []

headM :: [a] -> a
headM = foldl1(\x _ -> x)

lastM :: [a] -> a
lastM = foldr1(\_ x -> x)