reversePolish :: (Fractional a, Read a) => String -> a
reversePolish expression = head (foldl partialCalc [] (words expression))
    where partialCalc (one:two:stack) "*" = (one * two):stack
          partialCalc (one:two:stack) "/" = (two / one):stack
          partialCalc (one:two:stack) "+" = (one + two):stack
          partialCalc (one:two:stack) "-" = (two - one):stack
          partialCalc stack number = read number:stack