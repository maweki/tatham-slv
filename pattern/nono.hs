import Data.Traversable    (sequence)
import Prelude hiding      (sequence,(&&),(||),not,any,all)
import Data.Map            (Map,(!), toList)
import qualified Data.Map as Map
import Control.Monad.State (MonadState)
import Control.Monad (replicateM, forM_)

import Ersatz

build_runs :: [Int] -> [Bit] -> Bit
build_runs nums vars = case nums of
  [] -> not $ any id vars
  [0] -> not $ any id vars
  x:xs -> let max_d = length vars - (length xs + sum xs) - x
              run = [not] ++ replicate x id ++ [not]
              v = [false] ++ vars ++ [false]
          in any id $ for [0..max_d] $ \ d ->
              (all id $ zipWith id run $ take (x + 2) $ drop d v)
              && (build_runs xs $ drop (x + d + 1) vars)

count_correct :: Int -> [Bit] -> Bit
count_correct cnt vars = case vars of
    [] -> if cnt == 0 then true else false
    x:xs -> (x && count_correct (cnt - 1) xs) || (not x && count_correct cnt xs)

main = do
  input <- getContents
  let (xin, yin) = read input :: ([[Int]],[[Int]])
      sizex = length xin
      sizey = length yin
      indices = [(x, y) | x <- [1..sizex], y <- [1..sizey] ]

      nono :: (MonadState s m, HasSAT s) =>  m (Map (Int,Int) Bit)
      nono = do
        vars' <- replicateM (length indices) exists
        vars <- return $ Map.fromList $ zip indices vars'
        let getx x = for [1..sizey] $ \y -> (vars ! (x,y))
            gety y = for [1..sizex] $ \x -> (vars ! (x,y))
            build = \n v -> build_runs n v && count_correct (sum n) v
            xs = zipWith build xin (map getx [1..])
            ys = zipWith build yin (map gety [1..])
            main_conjunction = (all id xs) && (all id ys)
        assert (main_conjunction)
        return vars

  putStrLn $ "Size: " ++ show (sizex, sizey)

  (Satisfied, Just solution) <- solveWith minisat nono
  let lines = for [1..sizey] $ \i -> filter (\ ((x, y), b) -> y == i) $ toList solution
      conv  = map $ \((x,y), b)-> if b then 'X' else ' '
      join = foldr (:) ""
  forM_ (map (join . conv) lines) print

for = flip map
