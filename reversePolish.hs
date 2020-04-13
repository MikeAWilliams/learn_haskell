reversePolish :: (Num a, Read a) => String -> a
reversePolish expression = head (foldl partialCalc [] (words expression))
    where partialCalc (one:two:stack) "*" = (one * two):stack
          partialCalc stack number = read number:stack