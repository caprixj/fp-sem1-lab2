module Main where

import Gauss

import Control.Parallel.Strategies
import Text.Printf (printf)
import System.Clock
import Control.Exception (evaluate)

main :: IO ()
main = do
    let n = 500
    putStrLn $ "--- Parallel Gaussian elimination (Haskell) ---"
    putStrLn $ "Generating " ++ show n ++ "x" ++ show n ++ " matrix..."
    let matrix = generateMatrix n
            
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
    mapM_ (uncurry (printf "x_%d = %.2f" :: Int -> Double -> IO ())) (zip [1 :: Int ..] firstFiveSolutions)
    putStrLn ""