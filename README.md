## Gaussian Elimination in Haskell (Sequential vs Parallel)

This project implements Gaussian elimination to solve systems of linear equations in Haskell and compares two approaches:

- **Sequential**: standard `map` for row operations.
- **Parallel**: `parMap rdeepseq` from `Control.Parallel.Strategies` to parallelize row updates.

### Project Structure

- `library` (Cabal library): core Gaussian elimination logic
- `executable` (app/Main.hs): generates a matrix and benchmarks sequential vs parallel
- `test` (Hspec): correctness tests for the solvers

### Prerequisites

- GHC and Cabal installed

### Build

```bash
cabal build
```

Builds the library, executable, and tests.

### Run Tests

```bash
cabal test
```

Runs the Hspec suite to verify solver correctness.

### Run Performance Comparison

The main executable generates a large matrix (e.g., 500x500), runs both implementations, and prints timings.

Important: To benefit from parallelism, pass RTS `-N` flags to use multiple capabilities (threads).

Use all available cores:

```bash
cabal run gauss-parallel -- +RTS -N
```

Use an explicit number of threads (e.g., 4):

```bash
cabal run gauss-parallel -- +RTS -N4
```

### Notes

- The parallel version leverages `parMap rdeepseq` to parallelize the row update step during elimination.
- Speedups depend on matrix size, number of cores, and runtime settings.


