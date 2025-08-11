module Main (main) where

import Criterion.Main
import qualified STBenchmarks

main :: IO ()
main = defaultMain
  [ STBenchmarks.benchmarks
  ]