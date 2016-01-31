# Pattern/Nonograms

## execution

`cat examples/wiki | runghc nono.hs`

Tatham's notation is also supported:

`echo "5x5:2/1/1.1.1/3/4/3/2/3/1.1/3" | runghc nono.hs`


## requirements
* haskell language
* ersatz (lib)
* minisat

## approach

We transform the problem to SAT and solve it with minisat.
