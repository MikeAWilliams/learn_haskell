import Data.List

sigmundOne inSet = [item*32+30 | item <- inSet, item*32+30 < 3000]
sigmundTwo inSet = [item*58+44 | item <- inSet, item*58+44 < 3000]
sigmundSolve inSet = intersect (sigmundOne inSet) (sigmundTwo inSet)

sigOne inSet = [item * 32 + 30 | item <- inSet]
sigTwo inSet = [item * 58 + 44 | item <- inSet]
sigSolve inSet max = intersect [x | x <-(sigOne inSet), x < max] [y | y <-(sigTwo inSet), y< max]