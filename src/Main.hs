{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies, QuasiQuotes, DeriveGeneric, DeriveAnyClass #-}
module Main where

import System.Environment
import Model
import DecisionTree
import Classifier
import DOT
import QQ
import Data.Tree
import Text.Printf 

{-
 We support two different models, MPG and Adult. to load one or the other just change the signature of the load function, below.
-}
import AdultModel
import MPGModel

load :: IO [MPGModel]
load = loadModel

main :: IO ()
main = do  
    command <- getArgs
    case command of
      (h:t) -> do
         let action = lookup h dispatch
         case action of
           Just a -> a 
           Nothing -> invalidUsage
      []     -> invalidUsage
         

helpMessage :: String
helpMessage = [qq|Usage: decision-tree-classifer [ -validate | -show ] 
-validate:	Perform a k-fold validation of the data set (k=3)
-show:		Output the decision tree in DOT format for visualization
-check:		Perform a quick overfitting test to compare trained data vs validation data.
|]


dispatch :: [(String, (IO ()))]  
dispatch =  [ ("-validate",     validateModel)  
            , ("-show", showDotRepresentation)
            , ("-check", checkOverfitting)
            ]  

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid Arguments"
  help


help :: IO ()
help = putStrLn helpMessage



-- shows a DOT representation (using the entire model)
showDotRepresentation = do
  m <- load 
  let model = m 
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c
  putStrLn $ toDiGraph tree
  return ()

-- perform a k=3 fold validation of our model
validateModel = do
  m <- load 
  c <- makeCounter
  
  (series, res) <- (validate m purityFunction  decisionTreeAttributes 3 categoryFunction)

  printSeries series
  putStrLn $ show res
 
  return ()

printSeries series = do
  putStrLn "Series Data"
  putStrLn "---------------------------------------------"
  printf "%-10s %10s %10s %10s\n" "Step" "Accuracy" "FMeasure" "Error"
  mapM (\(((AccuracyResult _ a _), (AccuracyResult _ f _), (AccuracyResult _ e _)),pt) ->
          printf "%-10s %10.3f %10.3f %10.3f\n" (show pt ++ ".") a f e
       ) (zip series [1..])
  
printScores scores = do
  printSeries series
  putStrLn $ show res
    where
      series = seriesData [scores]
      res    = rollup [scores]

checkOverfitting = do
  m <- load
  m2 <- shuffle m
  let model = split m2 2
  let training = model !! 0
  let validation = model !! 1

  c <- makeCounter
  let attrs = decisionTreeAttributes training
  tree <- buildTree NoAttribute attrs training purityFunction  0 c

  -- train the tree and check the scores.
  putStrLn "Trained Tree Applied to Own Model"
  putStrLn "----------------------------------"
  printScores (score training tree categoryFunction)
  putStrLn "Trained Tree Applied to Unseen Model"
  putStrLn "------------------------------------"
  printScores (score validation tree categoryFunction)
  
  

getTree = do
  m <- load
  m2 <- shuffle m
  let model = split m2 2
  let training = model !! 0
  let validation = model !! 1

  c <- makeCounter
  let attrs = decisionTreeAttributes training
  tree <- buildTree NoAttribute attrs training purityFunction  0 c

  return (tree)

test1 = do
  m <- load
  m2 <- shuffle m
  let model = split m2 2
  let training = model !! 0
  let validation = model !! 1

  c <- makeCounter
  let attrs = decisionTreeAttributes training
  tree <- buildTree NoAttribute attrs training purityFunction  0 c
  return (toScores validation tree)
             
toScores examples tree = map (\i -> scoreClassification i categoryFunction ) (classifyInstances examples tree)
