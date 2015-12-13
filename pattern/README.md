# Pattern/Nongrams

## execution

`cat examples/wiki | runghc nono.hs`


## requirements
* haskell language
* ersatz (lib)
* minisat

## approach

We transform the problem to SAT and solve it with minisat.
