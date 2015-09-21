module Model where

import Text.ParserCombinators.Parsec
import Data.CSV
import Data.List.Split
import System.Random
import Data.Array.IO
import Control.Monad

class LoadableModel a where
  readModel  :: [[String]] -> [a]
  toInstance :: [String]   -> a
  split      :: [a] -> Int    -> [[a]]
  split bits folds = take folds $ chunksOf (div (length bits) folds) bits
  shuffle :: [a] -> IO [a]
  shuffle xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
      j <- randomRIO (i,n)
      vi <- readArray ar i
      vj <- readArray ar j
      writeArray ar j vi
      return vj
    where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs =  newListArray (1,n) xs

readCSVFile :: String -> IO [[String]]
readCSVFile f =  do
  result <- parseFromFile csvFile f
  case result of
    (Right r) -> return r
    (Left  r) -> error "Error while parsing CSV data"

 
shuffle' :: [a] -> IO [a]
shuffle' xs = do
  ar <- newArray n xs
  forM [1..n] $ \i -> do
    j <- randomRIO (i,n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
