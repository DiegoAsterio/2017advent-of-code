module Day2
  (solveDayTwoPartOne,
   solveDayTwoPartTwo
  ) where

import Control.Applicative

solveDayTwoPartOne :: String -> String
solveDayTwoPartOne input =
  let allLines = lines input
      allSep = map words allLines
      allNumbers = (pure (map read) <*> allSep) :: [[Int]]
      maxs = maximum <$> allNumbers 
      mins = minimum <$> allNumbers
      rowresult = getZipList $ (-) <$> ZipList maxs <*>ZipList mins
  in show $ sum rowresult

solveDayTwoPartTwo :: String -> String
solveDayTwoPartTwo input =
  let allLines = lines input
      allSep = map words allLines
      allNumbers = (pure (map read) <*> allSep) :: [[Int]]
      drs = (getZipList $ (\x y -> divMod <$> x <*> y) <$> ZipList allNumbers <*> ZipList allNumbers) :: [[(Int, Int)]]
      filtered = (filter (\(_,y) -> y == 0)) <$> drs
      filteredbis = (filter (\(x,_) -> x /= 1)) <$> filtered
      rowresult = (sum . (map fst)) <$> filteredbis
  in show $ sum rowresult
  
