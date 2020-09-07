{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as MUV
import Debug.Trace


{-# INLINE for #-}
for :: Applicative f => Int -> (Int -> Int -> Bool) -> Int -> (Int -> f ()) -> f ()
for s b e f = loop s
  where
    loop i = if b i e
      then f i *> loop (i + 1)
      else pure ()

{-# NOINLINE makeVec #-}
makeVec :: Int -> ST s (MUV.MVector s Int)
makeVec = MUV.new

main :: IO ()
main = print (runST (wrap undefined 100000000))

----------------------------------
-------- DEFINITION SET 1 --------
----------------------------------

-- | Using these definitions results in fast performance and constant allocation that
-- remains unaffected by changes in loop count specified in @main@.

{-# INLINE test #-}
test :: MUV.MVector s Int -> Int -> ST s ()
test r i =
  for 0 (<) i \j -> do
    v <- MUV.unsafeRead r 0
    MUV.unsafeWrite r 0 (v + j)

{-# NOINLINE test2 #-}
test2 :: MUV.MVector s Int -> Int -> ST s ()
test2 = test

{-# NOINLINE wrap #-}
wrap :: a -> Int -> ST s ()
wrap _ i = do
  mv <- makeVec 1
  test2 mv i
  v <- MUV.unsafeRead mv 0
  traceShow v $ pure ()


----------------------------------
-------- DEFINITION SET 2 --------
----------------------------------

-- | Using these definitions and profiling will result in a 20x increase in runtime
-- and ~1.6GB of allocation (which scales with the loop count specified in @main@).

-- {-# NOINLINE test #-}
-- test :: MUV.MVector s Int -> Int -> ST s ()
-- test r i = do
--   for 0 (<) i \j -> do
--     v <- MUV.unsafeRead r 0
--     MUV.unsafeWrite r 0 (v + j)

-- {-# NOINLINE wrap #-}
-- wrap :: a -> Int -> ST s ()
-- wrap _ i = do
--   mv <- makeVec 1
--   test mv i
--   v <- MUV.unsafeRead mv 0
--   traceShow v $ pure ()

