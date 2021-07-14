module Day1
  ( sumMatchNext,
    solveDayOnePartOne,
    sumHalfWithFirst,
    solveDayOnePartTwo
  ) where

import Data.Char

partOneFilter:: (Eq a, Num a) => [a] -> [a]
partOneFilter xs =
  let
    ys = tail xs ++ [head xs]
    xys = zip xs ys
  in fst <$> filter (uncurry (==)) xys
  
sumMatchNext :: (Eq a, Num a) => [a] -> a 
sumMatchNext = sum . partOneFilter

castToInt :: String -> [Int]
castToInt = map digitToInt

solveDayOne:: ([Int] -> Int) -> String -> String
solveDayOne f input =
  let allLines = lines input
      allLists = map castToInt allLines
      allSolutions = map f allLists
      result = unlines $ map show allSolutions
  in  result

solveDayOnePartOne :: String -> String
solveDayOnePartOne = solveDayOne sumMatchNext

partTwoFilter:: (Eq a, Num a) => [a] -> [a]
partTwoFilter xs =
  let
    half = length xs `div` 2
    ys = drop half xs ++ take half xs
    xys = zip xs ys
  in fst <$> filter (uncurry (==)) xys

sumHalfWithFirst :: (Eq a, Num a) => [a] -> a
sumHalfWithFirst = sum . partTwoFilter
  
solveDayOnePartTwo :: String -> String
solveDayOnePartTwo = solveDayOne sumHalfWithFirst


