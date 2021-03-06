import Data.Traversable    (sequence)
import Prelude hiding      (sequence,(&&),(||),not,any,all)
import qualified Prelude as P (all, (&&), not)
import Data.Map            (Map,(!), toList)
import Data.Tuple          (swap)
import qualified Data.Map as Map
import Control.Monad.State (MonadState)
import Control.Monad (replicateM, forM_, unless)
import System.Exit (exitFailure)
import Text.ParserCombinators.ReadP
import Data.Maybe

import Ersatz

-- Problem definition
type PSize = (Int, Int)
type Run = [[Int]]
type Runs = (Run, Run)
data Problem = Problem PSize Runs deriving Show

problem_size :: Problem -> PSize
problem_size (Problem size _) = size

consistent :: Problem -> Bool
consistent (Problem size (xin, yin)) = consistent' size xin P.&& consistent' (swap size) yin
consistent' (w, h) xs = (w == length xs) P.&& P.all (\x -> (sum x) + (length x - 1) <= h) xs

-- MAIN
main = do
  input <- getContents
  let parsed = parse_tat input
      problem = case parsed of
        Just p -> p
        Nothing -> let (xin, yin) = read input :: ([[Int]],[[Int]])
                   in Problem (length xin, length yin) (xin, yin)

  putStrLn $ "Size: " ++ (show $ problem_size problem)
  unless (consistent problem) $ do {putStrLn "Problem inconsitent"; exitFailure}

  (Satisfied, Just solution) <- solveWith minisat (nono problem)
  let sizey = snd $ problem_size problem
      lines = for [1..sizey] $ \i -> filter (\ ((x, y), b) -> y == i) $ toList solution
      conv  = map $ \((x,y), b)-> if b then 'X' else ' '
      join = foldr (:) ""
  forM_ (map (join . conv) lines) print

-- SAT encoding
build_runs :: [Int] -> [Bit] -> Bit
build_runs nums vars = case nums of
  [] -> all not vars
  [0] -> all not vars
  x:xs -> let max_d = length vars - (length xs + sum xs) - x
              run   = [not] ++ replicate x id ++ [not]
              v     = [false] ++ vars ++ [false]
          in any id $ for [0..max_d] $ \ d ->
             (all not $ take d vars)
             && (all id $ zipWith id run $ take (x + 2) $ drop d v)
             && (build_runs xs $ drop (x + d + 1) vars)

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

-- PARSER
parse_tat = listToMaybe . map fst . filter (null .snd) . readP_to_S r_problem

number' = many1 (choice (map char ['0'..'9']))

r_size :: ReadP PSize
r_size = do l <- number'
            char 'x'
            r <- number'
            return (read l, read r)

r_run :: ReadP [Int]
r_run = do { li <- sepBy number' (char '.'); return $ map read li }

r_runs :: ReadP [[Int]]
r_runs = sepBy r_run (char '/')

r_problem :: ReadP Problem
r_problem = do size <- r_size; char ':'
               runs <- r_runs
               skipSpaces; eof
               return $ Problem size (take (fst size) runs, drop (fst size) runs)
