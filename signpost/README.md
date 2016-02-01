# Signpost

## execution

Tatham's notation is the supported input:

`python3 signpost_sat.py 4x4:edefceha10aa1aea16agg`


## requirements
* python3
* minisat (for `signpost_sat.py`)

## approach

1. We transform the problem to SAT and solve it with minisat, outputting the cnf directly. `signpost_sat.py`
2. We interpret it as a graph problem and depth-search/backtrack the solution. `signpost_dhc.py`
