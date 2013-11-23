module SudokuSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import Sudoku

main :: IO()
main = hspec spec

spec :: Spec
spec = do
    describe "sudoku" $ do
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

reversibleForMatrix :: (Grid -> Grid) -> Grid -> Bool
reversibleForMatrix func matrix = (func $ func matrix) == matrix
