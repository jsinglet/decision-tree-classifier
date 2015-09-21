{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies, QuasiQuotes #-}
module Main where

import System.Environment
import Model 
import MPGModel
import DecisionTree
import Classifier
import DOT
import QQ
 
readAutoMPGCSVFile :: IO [[String]]
readAutoMPGCSVFile = readCSVFile "data/auto-mpg/auto-mpg.data"

loadModel :: IO [MPGModel]
loadModel = readAutoMPGCSVFile >>= \x -> return (readModel x)

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
|]


dispatch :: [(String, (IO ()))]  
dispatch =  [ ("-validate",     validateModel)  
            , ("-show", showDotRepresentation)
            ]  

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid Arguments"
  help


help :: IO ()
help = putStrLn helpMessage


-- shows a DOT representation (using the entire model)
showDotRepresentation = do
  m <- loadModel
  let model = m 
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c
  putStrLn $ toDiGraph tree
  return ()

-- perform a k=3 fold validation of our model
validateModel = do
  m <- loadModel
  c <- makeCounter

  res <- (validate m purityFunction  decisionTreeAttributes 3 categoryFunction)
  putStrLn $ show res

  return ()



