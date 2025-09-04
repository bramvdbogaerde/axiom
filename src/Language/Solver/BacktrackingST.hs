{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | A version of the ST monad that supports backtracking.
module Language.Solver.BacktrackingST
  ( -- * ST Monad
    ST, runST
    -- * References  
  , STRef, newSTRef, readSTRef, writeSTRef
    -- * Snapshotting
  , Snapshot, snapshot, restore
  ) where

import Control.Lens
import Control.Monad.Reader
import qualified Control.Monad.ST as ST
import Data.Array.ST
import qualified Data.STRef as STRef
import Unsafe.Coerce

-------------------------------------------------------------
-- Resizable mutable arrays in the ST monad, i.e., vectors
-------------------------------------------------------------

-- | Vector indexed by 'ST' monad s, and containing elements 'a'
data Vector s a = Vector
  { _vectorStore :: STRef.STRef s (STArray s Int a),
    _vectorLength :: STRef.STRef s Int,
    _vectorCap :: STRef.STRef s Int
  }

$(makeLenses ''Vector)

-- | Read a field from any record using a lens to an STRef
readField :: r -> Lens' r (STRef.STRef s b) -> ST.ST s b
readField record lens = STRef.readSTRef (record ^. lens)

-- | Write a field to any record using a lens to an STRef
writeField :: r -> Lens' r (STRef.STRef s b) -> b -> ST.ST s ()
writeField record lens = STRef.writeSTRef (record ^. lens)

-- | Modify a field in any record using a lens to an STRef
modifyField :: r -> Lens' r (STRef.STRef s b) -> (b -> b) -> ST.ST s ()
modifyField record lens f = readField record lens >>= writeField record lens . f

-- | Update an element at the given position
setElement :: Vector s a -> Int -> a -> ST.ST s ()
setElement v position element = do
  currentLength <- readField v vectorLength
  if position < currentLength
    then do
      store <- readField v vectorStore
      writeArray store position element
    else error $ "index out of bounds " ++ show position

-- | Grows a vector by doubling its current capacity.
growVector :: Vector s a -> ST.ST s ()
growVector v = do
  currentLength <- readField v vectorLength
  newCap <- modifyField v vectorCap (* 2) >> readField v vectorCap
  newStore <- newArray_ (0, newCap - 1)
  oldStore <- readField v vectorStore
  sequence_ [readArray oldStore i >>= writeArray newStore i | i <- [0 .. currentLength - 1]]
  writeField v vectorStore newStore

-- | Push a new element at the end of the vector, resizing it (by copy) if necessary
-- Returns the index where the element was stored
pushElement :: Vector s a -> a -> ST.ST s Int
pushElement v newElement = do
  currentLength <- readField v vectorLength
  currentCap <- readField v vectorCap
  -- TODO: check how vectors usually grow in more performant programming languages
  if currentLength == currentCap
    -- Vector is full: allocate a new array, copy the elements from the previous one
    -- and
    then (do growVector v >> pushElement v newElement)
    -- Just add the vector at the end, and update the current length
    else do
      store <- readField v vectorStore
      writeArray store currentLength newElement
      writeField v vectorLength (currentLength + 1)
      return currentLength

-- | Get an element at the given position
getElement :: Vector s a -> Int -> ST.ST s a
getElement v position = readField v vectorStore >>= flip readArray position

-- | Create a new vector with given initial capacity and default element
newVector :: Int -> a -> ST.ST s (Vector s a)
newVector capacity defaultElem
  | capacity <= 0 = error "Vector capacity must be positive"
  | otherwise = Vector <$> (newArray (0, capacity - 1) defaultElem >>= STRef.newSTRef)
                       <*> STRef.newSTRef 0
                       <*> STRef.newSTRef capacity

-- | Get the current length of a vector
getVectorLength :: Vector s a -> ST.ST s Int
getVectorLength v = readField v vectorLength

vectorElements :: Vector s a -> ST.ST s [a]
vectorElements v@Vector { .. } = do
  currentLength <- readField v vectorLength
  storage <- readField v vectorStore
  mapM (readArray storage) [0..currentLength-1]

-------------------------------------------------------------
-- Dynamically typed data
-------------------------------------------------------------

data CellValue where
  CellValue :: a -> CellValue

unsafeCastCellValue :: forall a. CellValue -> a
unsafeCastCellValue (CellValue a) = unsafeCoerce a

-------------------------------------------------------------
-- Our own ST
-------------------------------------------------------------

-- | Internal data stored for each snapshot
data SnapshotData s = SnapshotData
  { snapshotOffset :: !Int
  , snapshotElems :: ![CellValue]
  }

instance Functor SnapshotData where
  fmap _ (SnapshotData offset elems) = SnapshotData offset elems

-- | Internal state of our ST monad, included the vector based storage
data STState s = STState
  { _storage :: Vector s CellValue
  , _snapshots :: Vector s (SnapshotData s)
  }

$(makeLenses ''STState)

-- | Our ST monad uses the real ST monad but captures a reference to our
-- internal state so that we can create references that are backed by elements
-- in the vector.
newtype ST s a = ST (ReaderT (STRef.STRef s (STState s)) (ST.ST s) a)
  deriving (Functor, Applicative, Monad)

-- | Create initial ST state with empty vector storage
initialSTState :: ST.ST s (STRef.STRef s (STState s))
initialSTState = STRef.newSTRef =<<
  STState <$> newVector 16 (CellValue ())
          <*> newVector 4 (SnapshotData 0 [])  -- Initial snapshots vector

runST :: (forall s . ST s a) -> a
runST computation = ST.runST $ do
  stateRef <- initialSTState
  let (ST reader) = computation
  runReaderT reader stateRef

-------------------------------------------------------------
-- Typed references
-------------------------------------------------------------

-- | A pointer-like value that can be imperatively updated.
data STRef s a
  = STRef
      -- | internal offet in the storage pointing to the value
      !Int
      -- | reference to the backing storage
      !(STRef.STRef s (STState s))
  deriving (Eq)

-- | Create a new reference
newSTRef :: a -> ST s (STRef s a)
newSTRef value = ST $
  STRef <$> (ask >>= lift . STRef.readSTRef >>= lift . flip pushElement (CellValue value) . (^. storage))
        <*> ask

-- | Read from the given reference
readSTRef :: STRef s a -> ST s a
readSTRef (STRef offset stateRef) = ST $ do
  state <- lift $ STRef.readSTRef stateRef
  let vec = state ^. storage
  cellValue <- lift $ getElement vec offset
  -- SAFETY: This cast is safe because we maintain the invariant that each
  -- STRef offset always corresponds to the same type that was originally
  -- stored at that position via newSTRef.
  return $ unsafeCastCellValue cellValue

-- | Write to the given reference
writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef offset stateRef) value = ST $ do
  storage <- fmap _storage $ lift $ STRef.readSTRef stateRef
  lift $ setElement storage offset (CellValue value)

-------------------------------------------------------------
-- Snapshotting
-------------------------------------------------------------

-- | A snapshot represents a saved state that can be restored later
data Snapshot s = Snapshot
  { _snapshotOffset :: !Int
  , _snapshotStateRef :: !(STRef.STRef s (STState s))
  }

-- | Create a snapshot of the current state
snapshot :: ST s (Snapshot s)
snapshot = ST $ do
  stateRef <- ask
  state <- lift $ STRef.readSTRef stateRef
  let sto = state ^. storage
  snapshotData <- SnapshotData <$> lift (getVectorLength sto)
                               <*> lift (vectorElements sto)
  Snapshot <$> lift (pushElement (state ^. snapshots) snapshotData)
           <*> pure stateRef

-- | Restore the state to a previously created snapshot
restore :: Snapshot s -> ST s ()
restore (Snapshot offset stateRef) = ST $ do
  state <- lift $ STRef.readSTRef stateRef
  snapshotData <- lift $ getElement (state ^. snapshots) offset
  -- NOTE: The capacity field does not need to be updated because the vector
  -- will always have sufficient capacity to hold the restored contents. Snapshots
  -- can only restore to a previous state, and the vector only grows, never shrinks.
  lift $ writeField (state ^. storage) vectorLength (snapshotOffset snapshotData)
  lift $ mapM_ (uncurry $ setElement (state ^. storage)) (zip [0..] (snapshotElems snapshotData))

-- TODO: Add cleanup functions to remove old snapshots


