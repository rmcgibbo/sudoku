# sudoku

This is a new package created by [Haskeleton][]. To finish setting it up, look
for "TODO"s in the Cabal file and in the library itself. To quickly get up and
running, simply run `make`. See below for more detailed instructions.

[haskeleton]: http://taylor.fausak.me/haskeleton/

``` sh
# Update Cabal's list of packages.
cabal update

# Initialize a sandbox and install the package's dependencies.
make install

# Configure & build the package.
make configure
make build

# Test package.
make test

# Benchmark package.
make bench

# Run executable.
make run

# Start REPL.
make repl

# Generate documentation.
make haddock

# Analyze coverage.
make hpc
```
