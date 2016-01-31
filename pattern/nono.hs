import Data.Traversable    (sequence)
import Prelude hiding      (sequence,(&&),(||),not,any,all)
import Data.Map            (Map,(!), toList)
import qualified Data.Map as Map
import Control.Monad.State (MonadState)
import Control.Monad (replicateM, forM_)

import Ersatz

type PSize = (Int, Int)
type Run = [[Int]]
type Runs = (Run, Run)
data Problem = Problem PSize Runs deriving Show

problem_size :: Problem -> PSize
problem_size (Problem size _) = size

build_runs :: [Int] -> [Bit] -> Bit
build_runs nums vars = case nums of
  [] -> all not vars
  [0] -> all not vars
  x:xs -> let max_d = length vars - (length xs + sum xs) - x
              run = [not] ++ replicate x id ++ [not]
              v = [false] ++ vars ++ [false]
          in any id $ for [0..max_d] $ \ d ->
              (all not $ take d vars)
              && (all id $ zipWith id run $ take (x + 2) $ drop d v)
              && (build_runs xs $ drop (x + d + 1) vars)


main = do
  input <- getContents
  let (xin, yin) = read input :: ([[Int]],[[Int]])
      problem = Problem (length xin, length yin) (xin, yin)

  putStrLn $ "Size: " ++ (show $ problem_size problem)

  (Satisfied, Just solution) <- solveWith minisat (nono problem)
  let sizey = snd $ problem_size problem
      lines = for [1..sizey] $ \i -> filter (\ ((x, y), b) -> y == i) $ toList solution
      conv  = map $ \((x,y), b)-> if b then 'X' else ' '
      join = foldr (:) ""
  forM_ (map (join . conv) lines) print

nono :: (MonadState s m, HasSAT s) => Problem -> m (Map (Int,Int) Bit)
nono (Problem (sizex, sizey) (xin, yin)) = do
  let indices = [(x, y) | x <- [1..sizex], y <- [1..sizey] ]
  vars' <- replicateM (length indices) exists
  vars <- return $ Map.fromList $ zip indices vars'
  let getx x = for [1..sizey] $ \y -> (vars ! (x,y))
      gety y = for [1..sizex] $ \x -> (vars ! (x,y))
      xs = zipWith build_runs xin (map getx [1..])
      ys = zipWith build_runs yin (map gety [1..])
  assert (all id xs && all id ys)
  return vars

for = flip map
