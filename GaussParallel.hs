module Main where

import Control.Parallel.Strategies
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Printf (printf)

-- forward gaussian elimination
-- all rows that come after k are updated in parallel
forwardStep :: [[Double]] -> Int -> [[Double]]
forwardStep matrix k =
    let n = length matrix
        (top, rest) = splitAt k matrix
        (pivotRow : rowsToUpdate) = rest
        pivotVal = pivotRow !! k

        updateRow :: [Double] -> [Double]
        updateRow targetRow =
            let factor = (targetRow !! k) / pivotVal
            in zipWith (\p t -> t - factor * p) pivotRow targetRow
        
        updatedRows :: [[Double]]
        updatedRows = parMap rdeepseq updateRow rowsToUpdate
        
    in top ++ (pivotRow : updatedRows)


-- back substitution (solving a system with an upper-triangular matrix)
backSub :: [[Double]] -> [Double]
backSub matrix = foldr solveRow [] matrix
  where
    n = length matrix
    
    solveRow :: [Double] -> [Double] -> [Double]
    solveRow row knownXs =
        let (coeffs, [rhs]) = splitAt n row
            i = n - 1 - length knownXs
            knownCoeffs = drop (i + 1) coeffs
            sumKnown = sum $ zipWith (*) knownCoeffs knownXs
            diagCoeff = coeffs !! i
            x_i = (rhs - sumKnown) / diagCoeff
            
        in x_i : knownXs


-- main function for solving the linear system
solveGauss :: [[Double]] -> [Double]
solveGauss matrix =
    let n = length matrix
        eliminatedMatrix = foldl' forwardStep matrix [0..n-1]

    in backSub eliminatedMatrix


main :: IO ()
main = do
    -- example system:
    -- 2x + 1y - 1z = 8
    -- -3x - 1y + 2z = -11
    -- -2x + 1y + 2z = -3
    
    -- solution: x=2, y=3, z=-1

    let matrix = [ [ 2,  1, -1,   8],
                   [-3, -1,  2, -11],
                   [-2,  1,  2,  -3] ]
                   
    putStrLn "--- Parallel Gaussian elimination (Haskell) ---"
    putStrLn "\nInitial augmented matrix (A|b):"
    mapM_ print matrix

    let solutions = solveGauss matrix
    
    putStrLn "\nSolution:"
    mapM_ (uncurry (printf "x_%d = %.2f\n" :: Int -> Double -> IO ())) (zip [1 :: Int ..] solutions)
    putStrLn ""
    