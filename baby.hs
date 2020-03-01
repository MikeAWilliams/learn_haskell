doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber x = if x > 100
                       then x
                       else x*2

helloWorld = "hello" ++ ' ':"world"

h = helloWorld!!0

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
rightTriangles = [(a,b,c) | c <- [1..10] , b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == 24]