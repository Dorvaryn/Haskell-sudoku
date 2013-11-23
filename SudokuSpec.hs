module SudokuSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import Sudoku

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "base utils" $ do
        it "rows is reversible" $ do
            property $ reversibleForMatrix rows
        it "cols is reversible" $ do
            reversibleForMatrix cols easy `shouldBe` True
            reversibleForMatrix cols try `shouldBe` True
            -- TODO: add an Arbitrary instance to generate valid matrixs : property $ reversibleForMatrix cols
        it "boxs is reversible" $ do
            reversibleForMatrix boxs easy `shouldBe` True
            reversibleForMatrix boxs try `shouldBe` True
            -- TODO: add an Arbitrary instance to generate valid matrixs : property $ reversibleForMatrix boxs
    describe "sudoku" $ do
        it "solve easy" $ do
            solve easy `shouldBe` resultEasy
        it "solveFast easy" $ do
            solveFast easy `shouldBe` resultEasy
        it "solveFast try" $ do
            solveFast try `shouldBe` resultTry

reversibleForMatrix :: (Grid -> Grid) -> Grid -> Bool
reversibleForMatrix func matrix = (func $ func matrix) == matrix

resultEasy = [["249571638","861432975","573986142","725698413","698143257","314725869","937814526","152369784","486257391"]]
resultTry = [["295743861","431865927","876192543","387459216","612387495","549216738","763524189","928671354","154938672"]]
