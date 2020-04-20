reversePolish :: (Fractional a, Read a) => String -> a
reversePolish expression = head (foldl partialCalc [] (words expression))
    where partialCalc (one:two:stack) "*" = (one * two):stack
          partialCalc (one:two:stack) "/" = (two / one):stack
          partialCalc (one:two:stack) "+" = (one + two):stack
          partialCalc (one:two:stack) "-" = (two - one):stack
          partialCalc stack number = read number:stack

twoList = replicate 1000000 " 2 + "
twos = concat twoList
testString = "2 2 +" ++ twos 
-- :set +s
-- reversePoliish testString
-- 2000004.0 
-- (3.45 secs, 5,195,872,968 bytes)