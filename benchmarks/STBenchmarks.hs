module STBenchmarks (benchmarks) where

import Criterion.Main
import qualified Language.Solver.BacktrackingST as BST
import qualified Control.Monad.ST as ST
import qualified Data.STRef as STRef
import System.Random
import Text.Printf

-- | Sizes to benchmark
benchSizes :: [Int]
benchSizes = [100, 500, 1000, 10000, 50000]

-- | Benchmark group for ST implementations
benchmarks :: Benchmark
benchmarks = bgroup "ST Implementations"
  [ bgroup "Multiple References" $
    concatMap (\size ->
      [ bench (printf "BacktrackingST %d refs" size) $ nfIO (benchBacktrackingMultiple size)
      , bench (printf "Standard ST %d refs" size) $ nfIO (benchStandardMultiple size)
      ]) benchSizes
  , bgroup "Sequential Updates" $
    map (\size ->
      bgroup (printf "%d updates" size)
        [ bench "BacktrackingST" $ nfIO (benchBacktrackingSequential size)
        , bench "Standard ST" $ nfIO (benchStandardSequential size)
        ]) benchSizes
  ]
-- Multiple references benchmark for BacktrackingST
benchBacktrackingMultiple :: Int -> IO [Int]
benchBacktrackingMultiple n = return $ BST.runST $ do
  refs <- sequence [BST.newSTRef i | i <- [1..n]]
  mapM BST.readSTRef refs

-- Multiple references benchmark for standard ST
benchStandardMultiple :: Int -> IO [Int]
benchStandardMultiple n = return $ ST.runST $ do
  refs <- sequence [STRef.newSTRef i | i <- [1..n]]
  mapM STRef.readSTRef refs

-- Sequential updates benchmark for BacktrackingST
benchBacktrackingSequential :: Int -> IO Int
benchBacktrackingSequential n = return $ BST.runST $ do
  ref <- BST.newSTRef (0 :: Int)
  sequence_ [BST.writeSTRef ref i | i <- [1..n]]
  BST.readSTRef ref

-- Sequential updates benchmark for standard ST
benchStandardSequential :: Int -> IO Int
benchStandardSequential n = return $ ST.runST $ do
  ref <- STRef.newSTRef (0 :: Int)
  sequence_ [STRef.writeSTRef ref i | i <- [1..n]]
  STRef.readSTRef ref
