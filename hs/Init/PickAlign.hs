module Init.PickAlign
  ( Tag(..)
  , alignScore
  ) where

import Data.Array (Array, array, (!))


data Tag = TagMatch | TagSub | TagIns deriving (Eq, Show)

data Step = StepNone | StepMatch | StepSub | StepIns | StepDel deriving (Eq, Show)

type Cell = (Int, Step)

alignScore :: String -> String -> (Int, [Tag])
alignScore needle haystack = (cost, tags)
  where
    n = length needle
    m = length haystack
    delCost = 100
    insCost = 1
    subCost a b = if a == b then 0 else 100

    dp :: Array (Int, Int) Cell
    dp = array ((0, 0), (n, m))
      [ ((i, j), cell i j) | i <- [0 .. n], j <- [0 .. m] ]

    cell 0 0 = (0, StepNone)
    cell 0 j = (j * insCost, StepIns)
    cell i 0 = (i * delCost, StepDel)
    cell i j = chooseBest
      where
        (cDel, _) = dp ! (i - 1, j)
        (cIns, _) = dp ! (i, j - 1)
        (cSub, _) = dp ! (i - 1, j - 1)
        costDel = cDel + delCost
        costIns = cIns + insCost
        costSub = cSub + subCost (needle !! (i - 1)) (haystack !! (j - 1))
        bestCost = minimum [costSub, costIns, costDel]
        chooseBest
          | bestCost == costSub && needle !! (i - 1) == haystack !! (j - 1) = (bestCost, StepMatch)
          | bestCost == costSub = (bestCost, StepSub)
          | bestCost == costIns = (bestCost, StepIns)
          | otherwise = (bestCost, StepDel)

    (cost, _) = dp ! (n, m)
    tags = backtrace n m []

    backtrace 0 0 acc = acc
    backtrace i j acc =
      case snd (dp ! (i, j)) of
        StepMatch -> backtrace (i - 1) (j - 1) (TagMatch : acc)
        StepSub -> backtrace (i - 1) (j - 1) (TagSub : acc)
        StepIns -> backtrace i (j - 1) (TagIns : acc)
        StepDel -> backtrace (i - 1) j acc
        StepNone -> acc
