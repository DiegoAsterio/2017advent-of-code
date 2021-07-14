module Day2
  (solveDayTwoPartOne
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
      
  
  
