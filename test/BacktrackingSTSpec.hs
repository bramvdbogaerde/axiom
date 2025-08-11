module BacktrackingSTSpec (spec) where

import Language.Solver.BacktrackingST
import Test.Hspec

spec :: Spec
spec = describe "BacktrackingST" $ do
  describe "STRef operations" $ do
    it "creates and reads references" $ do
      let result = runST $ do
            ref <- newSTRef (42 :: Int)
            readSTRef ref
      result `shouldBe` 42

    it "writes to references" $ do
      let result = runST $ do
            ref <- newSTRef (0 :: Int)
            writeSTRef ref 100
            readSTRef ref
      result `shouldBe` 100

    it "handles multiple references independently" $ do
      let result = runST $ do
            ref1 <- newSTRef (10 :: Int)
            ref2 <- newSTRef (20 :: Int)
            writeSTRef ref1 15
            val1 <- readSTRef ref1
            val2 <- readSTRef ref2
            return (val1, val2)
      result `shouldBe` (15, 20)

  describe "Type safety" $ do
    it "maintains type safety across different types" $ do
      let result = runST $ do
            intRef <- newSTRef (42 :: Int)
            stringRef <- newSTRef ("hello" :: String)
            boolRef <- newSTRef (True :: Bool)
            
            intVal <- readSTRef intRef
            stringVal <- readSTRef stringRef  
            boolVal <- readSTRef boolRef
            
            return (intVal, stringVal, boolVal)
      result `shouldBe` (42, "hello", True)

  describe "Sequential operations" $ do
    it "handles many sequential operations" $ do
      let result = runST $ do
            ref <- newSTRef (0 :: Int)
            writeSTRef ref 1
            writeSTRef ref 2
            writeSTRef ref 3
            readSTRef ref
      result `shouldBe` 3

    it "handles growing storage" $ do
      let result = runST $ do
            refs <- sequence [newSTRef i | i <- [1..50] :: [Int]]
            mapM readSTRef refs
      result `shouldBe` [1..50]

  describe "Snapshotting" $ do
    it "creates and restores snapshots" $ do
      let result = runST $ do
            ref <- newSTRef (10 :: Int)
            writeSTRef ref 20
            snap <- snapshot
            writeSTRef ref 30
            val1 <- readSTRef ref
            restore snap
            val2 <- readSTRef ref
            return (val1, val2)
      result `shouldBe` (30, 20)

    it "handles multiple snapshots independently" $ do
      let result = runST $ do
            ref1 <- newSTRef (1 :: Int)
            ref2 <- newSTRef (2 :: Int)
            snap1 <- snapshot
            writeSTRef ref1 10
            writeSTRef ref2 20
            snap2 <- snapshot
            writeSTRef ref1 100
            writeSTRef ref2 200
            -- Restore to snap2
            restore snap2
            vals1 <- (,) <$> readSTRef ref1 <*> readSTRef ref2
            -- Restore to snap1
            restore snap1
            vals2 <- (,) <$> readSTRef ref1 <*> readSTRef ref2
            return (vals1, vals2)
      result `shouldBe` ((10, 20), (1, 2))

    it "works with multiple references" $ do
      let result = runST $ do
            refs <- sequence [newSTRef i | i <- [1..10] :: [Int]]
            snap <- snapshot
            sequence_ [writeSTRef ref (i * 10) | (i, ref) <- zip [1..] refs]
            vals1 <- mapM readSTRef refs
            restore snap
            vals2 <- mapM readSTRef refs
            return (vals1, vals2)
      result `shouldBe` ([10,20,30,40,50,60,70,80,90,100], [1,2,3,4,5,6,7,8,9,10])