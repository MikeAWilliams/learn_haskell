data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

testData :: RoadSystem  
testData = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path)  
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
    in  (newPathToA, newPathToB)  