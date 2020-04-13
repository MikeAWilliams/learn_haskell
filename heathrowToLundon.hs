import Data.List  

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

testData :: RoadSystem  
testData = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)  
roadStep (pathA, pathB, lenA, lenB) (Section a b c) =   
    let forwardPriceToA = lenA + a  
        crossPriceToA = lenB + b + c  
        forwardPriceToB = lenB + b  
        crossPriceToB = lenA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA
        newLenA = if forwardPriceToA <= crossPriceToA
                        then forwardPriceToA
                        else crossPriceToA
        newLenB = if forwardPriceToB <= crossPriceToB
                        then forwardPriceToB
                        else crossPriceToB
    in  (newPathToA, newPathToB, newLenA, newLenB)  

optimalPath :: RoadSystem -> (Path, Int)
optimalPath roadSystem =
    let (bestAPath, bestBPath, lenA, lenB) = foldl roadStep ([], [], 0, 0) roadSystem
    in if lenA <= lenB
        then (reverse bestAPath, lenA)
        else (reverse bestBPath, lenB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n list = take n list : groupsOf n (drop n list)

main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        (path, len) = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show len  