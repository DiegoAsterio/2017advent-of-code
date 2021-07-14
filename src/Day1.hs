module Day1
  ( sumMatchNext,
    solveDayOnePartOne,
    sumHalfWithFirst,
    solveDayOnePartTwo
  ) where

import Data.Char

sumMatchNext :: (Eq a, Num a) => [a] -> a 
sumMatchNext xs = snd (foldr (\x (ans, cum) -> (x, cum + (if ans == x then x else 0))) (head xs, 0) xs)

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

sumHalfWithFirst :: (Eq a, Num a) => [a] -> a
sumHalfWithFirst xs =
  let
    half = length xs `div` 2
    ys = drop half xs ++ take half xs
  in foldl (\acc (x, y) -> acc + if x == y then x else 0) 0 $ zip xs ys
  
solveDayOnePartTwo :: String -> String
solveDayOnePartTwo = solveDayOne sumHalfWithFirst


