module Tests where

import Model 
import MPGModel
import DecisionTree
import Classifier
import DOT



test1 = do
  m <- loadModel
  let model = m -- (split m 3) !! 0
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c
  putStrLn $ show tree
  return ()

test2 = do
  m <- loadModel
  let model = m -- (split m 3) !! 0
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c
  putStrLn $ toDiGraph tree
  return ()

test3 = do
  m <- loadModel
  let model = m -- (split m 3) !! 0
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c

  -- classify an instance
  putStrLn $ show (classifyInstance tree (head model))
 
  return ()


test4 = do
  m <- loadModel
  let model = m -- (split m 3) !! 0
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c

  putStrLn $ show (score model tree categoryFunction)
 
  return ()

test5 = do
  m <- loadModel
  let model = m -- (split m 3) !! 0
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c

  putStrLn $ show (findKinds FalsePositive  model tree categoryFunction)
 
  return ()

test6 = do
  m <- loadModel
  let model = m
  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs model purityFunction  0 c

  putStrLn $ show $ head (findKinds FalseNegative  model tree categoryFunction)
 
  return ()

test7 = do
  m <- loadModel
  -- shuffle the model 
  shuffled <- shuffle m
  let trainingModel = (split shuffled 3) !! 0
  let validationModel = (split shuffled 3) !! 1
  let devModel = (split shuffled 3) !! 2

  c <- makeCounter
  let attrs = decisionTreeAttributes m
  tree <- buildTree NoAttribute attrs trainingModel purityFunction  0 c

  --putStrLn $ show $ head (findKinds FalseNegative  validationModel tree categoryFunction)
  putStrLn $ ("validationModel:") ++ show (score validationModel tree categoryFunction)
  putStrLn $ ("devModel:") ++ show (score devModel tree categoryFunction)

  return ()


test8 = do
  m <- loadModel
  c <- makeCounter

  res <- (validate m purityFunction  decisionTreeAttributes 3 categoryFunction)
  putStrLn $ show res

  return ()



