module Lib where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.Functor
import System.Random
import System.TimeIt

transactionRepeats :: Int
transactionRepeats = 100

writeQ :: TBQueue Int -> IO ()
writeQ q = void $ replicateM transactionRepeats $ do
  atomically $ do
    writeTBQueue q 0
  val <- randomRIO (1, 100)
  threadDelay val

readQ :: TBQueue Int -> IO ()
readQ q = void $ replicateM transactionRepeats $ do
  atomically $ do
    _ <- readTBQueue q
    return ()
  val <- randomRIO (1, 100)
  threadDelay val

peekQ :: TBQueue Int -> IO ()
peekQ q = void $ replicateM transactionRepeats $ do
  atomically $ do
    _ <- peekTBQueue q
    return ()
  val <- randomRIO (1, 100)
  threadDelay val

asyncCount :: Int
asyncCount = 10

burn :: IO ()
burn = do
  tbq <- newTBQueueIO 10
  forever $ timeIt $ do
    _ <- replicateM asyncCount (async (writeQ tbq))
    asyncs <- replicateM asyncCount (async (peekQ tbq))
    _ <- replicateM asyncCount (async (writeQ tbq))
    asyncs <- replicateM asyncCount (async (readQ tbq))
    wait (head asyncs)
    putStrLn "Successfully waited"
