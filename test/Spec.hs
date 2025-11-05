module Main (main) where

import Test.Hspec
import Gauss

epsilon :: Double
epsilon = 1e-9

-- checks if two lists of Doubles are "close enough"
solutionsClose :: [Double] -> [Double] -> Bool
solutionsClose xs ys =
    length xs == length ys &&
    all (\(x, y) -> abs (x - y) < epsilon) (zip xs ys)

main :: IO ()
main = hspec $ do
    -- test suite for the whole gaussian elimination process
    describe "Gauss.solveGauss" $ do

        it "solves a simple 3x3 system" $ do
            let matrix = [ [2, 1, -1, 8]
                         , [-3, -1, 2, -11]
                         , [-2, 1, 2, -3]
                         ]
            -- expected solution: x=2, y=3, z=-1
            let expected = [2.0, 3.0, -1.0]
            solveGauss matrix `shouldSatisfy` solutionsClose expected

        it "solves the generated 3x3 diagonal matrix" $ do
            let matrix = generateMatrix 3
            let expected = [0.0, 1.0, 2.0]
            solveGauss matrix `shouldSatisfy` solutionsClose expected

        it "gives the same result as the sequential version" $ do
            let matrix = generateMatrix 50
            let solPar = solveGauss matrix
            let solSeq = solveGaussSequential matrix
            solPar `shouldSatisfy` solutionsClose solSeq

    -- test for an isolated part
    describe "Gauss.backSub" $ do
        it "solves a pre-eliminated 3x3 system" $ do
            let upperTriangular = [ [2, 1, -1, 8]
                                  , [0, 0.5, 0.5, 1.0]
                                  , [0, 0, -1, 1]
                                  ]
            -- expected solution: x=2, y=3, z=-1
            let expected = [2.0, 3.0, -1.0]
            backSub upperTriangular `shouldSatisfy` solutionsClose expected