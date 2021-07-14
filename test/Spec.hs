import Test.Hspec
import Day1
import Day2

main :: IO ()
main = hspec $ do
  describe "Day 1: Part 1" $ do
    it "1122 produces 3" $ do
      sumMatchNext [1, 1, 2, 2] `shouldBe` (3 :: Int)
    it "1111 produces 4" $ do
      sumMatchNext [1, 1, 1, 1] `shouldBe` (4 :: Int)
    it "1234 produces 0" $ do
      sumMatchNext [1, 2, 3, 4] `shouldBe` (0 :: Int)
    it "91212129 produces 9" $ do
      sumMatchNext [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` (9 :: Int)
  describe "Day 1: Part 2" $ do
    it "1212 produces 6" $ do
      sumHalfWithFirst [1, 2, 1, 2] `shouldBe` (6 :: Int)
    it "1221 produces 0" $ do
      sumHalfWithFirst [1, 2, 2, 1] `shouldBe` (0 :: Int)
    it "123425 produces 4" $ do
      sumHalfWithFirst [1, 2, 3, 4, 2, 5] `shouldBe` (4 :: Int)
    it "123123 produces 12" $ do
      sumHalfWithFirst [1, 2, 3, 1, 2, 3] `shouldBe` (12 :: Int)
    it "12131415 produces 4" $ do
      sumHalfWithFirst [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` (4 :: Int)
  describe "Day 2: Part 1" $ do
    it "5 1 9 5\n  7 5 3\n  2 4 6 8\n  produces 3" $ do
      solveDayTwoPartOne "5 1 9 5\n7 5 3\n2 4 6 8\n" `shouldBe` "18"

