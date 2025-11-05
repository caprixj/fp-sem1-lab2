module Gauss
    ( forwardStep
    , forwardStepSequential
    , backSub
    , solveGauss
    , solveGaussSequential
    , generateMatrix
    , timeSpecToSeconds
    ) where

import Control.Parallel.Strategies
import Data.List (foldl')
import System.Environment (getArgs)
import Text.Printf (printf)
import System.Clock
import Control.Exception (evaluate)

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


forwardStepSequential :: [[Double]] -> Int -> [[Double]]
forwardStepSequential matrix k =
    let n = length matrix
        (top, rest) = splitAt k matrix
        (pivotRow : rowsToUpdate) = rest
        pivotVal = pivotRow !! k

        updateRow :: [Double] -> [Double]
        updateRow targetRow =
            let factor = (targetRow !! k) / pivotVal
            in zipWith (\p t -> t - factor * p) pivotRow targetRow
        
        updatedRows :: [[Double]]
        -- updatedRows = parMap rdeepseq updateRow rowsToUpdate
        updatedRows = map updateRow rowsToUpdate
        
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


solveGaussSequential :: [[Double]] -> [Double]
solveGaussSequential matrix =
    let n = length matrix
        -- eliminatedMatrix = foldl' forwardStep matrix [0..n-1]
        eliminatedMatrix = foldl' forwardStepSequential matrix [0..n-1]

    in backSub eliminatedMatrix


timeSpecToSeconds :: TimeSpec -> Double
timeSpecToSeconds ts = fromIntegral (sec ts) + fromIntegral (nsec ts) / 1000000000.0


-- generates a simple solvable n x (n+1) matrix
generateMatrix :: Int -> [[Double]]
generateMatrix n =
    [ [ (if i == j then 1.0 else 0.0) | j <- [0..n-1] ] ++ [fromIntegral i] | i <- [0..n-1] ]
    -- this creates a diagonal matrix where:
    -- 1x_0 + 0x_1 + ... = 0  (solution x_0 = 0)
    -- 0x_0 + 1x_1 + ... = 1  (solution x_1 = 1)
    -- ...
    -- solution is [0.0, 1.0, 2.0, ..., n-1]


main :: IO ()
main = do
    let n = 500
    putStrLn $ "--- Parallel Gaussian elimination (Haskell) ---"
    putStrLn $ "Generating " ++ show n ++ "x" ++ show n ++ " matrix..."
    let matrix = generateMatrix n
                   
    -- putStrLn "\nInitial augmented matrix (A|b):"
    -- mapM_ print matrix

    -- sequential
    seqStartTime <- getTime Monotonic
    let solutionsSeq = solveGaussSequential matrix
    evaluate (solutionsSeq `using` rdeepseq)
    seqEndTime <- getTime Monotonic
    let seqTime = diffTimeSpec seqEndTime seqStartTime

    -- parallel
    parStartTime <- getTime Monotonic
    let solutions = solveGauss matrix
    evaluate (solutions `using` rdeepseq)
    parEndTime <- getTime Monotonic
    let parTime = diffTimeSpec parEndTime parStartTime

    -- time comparison
    putStrLn "\n--- Execution Time Comparison ---"
    printf "Sequential Version: %.4f sec\n" (timeSpecToSeconds seqTime)
    printf "Parallel Version:   %.4f sec\n" (timeSpecToSeconds parTime)

    -- solution
    putStrLn "\nSolution (from parallel exec, first 5 elements):"
    let firstFiveSolutions = take 5 solutions
    mapM_ (uncurry (printf "x_%d = %.2f\n" :: Int -> Double -> IO ())) (zip [1 :: Int ..] firstFiveSolutions)
    putStrLn ""
