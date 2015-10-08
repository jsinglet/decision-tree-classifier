{-# LANGUAGE OverloadedStrings #-}
module Classifier where

import Data.Tree
import DecisionTree hiding (classify)
import Debug.Trace
import Model
import Data.List
import Stats
import Text.Printf

{-
  Used for making guesses about instances
-}
type Explanation    = [String]
data Classification a = Classification StopCondition a Explanation deriving (Show)
data ClassificationClass = TruePositive | TrueNegative | FalsePositive | FalseNegative deriving (Show,Eq)
data ClassificationScore a = ClassificationScore (Classification a) ClassificationClass deriving (Show)

data ClassifierAccuracy = ClassifierAccuracy [AccuracyResult] deriving (Eq)

data AccuracyMeasure =
  Precision
  | Recall
  | FMeasure
  | Informal
  | Accuracy
  deriving (Show,Eq)

-- show (AccuracyResult Precision v (a, b, c)) = "Precision: (" ++ (show v) ++ ")  " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)

labelMean :: String
labelMean = "mean"
labelMedian :: String
labelMedian = "median"
labelSTDDev :: String
labelSTDDev = "stddev"
labelBlank  :: String
labelBlank = ""
labelValue :: String
labelValue = "value"
labelPrecision :: String
labelPrecision = "Precision"
labelRecall :: String
labelRecall    = "Recall"
labelFMeasure :: String
labelFMeasure  = "FMeasure"
labelAccuracy :: String
labelAccuracy =  "Accuracy"
labelInformal :: String
labelInformal = "Informal"

data AccuracyResult = AccuracyResult AccuracyMeasure Double (Double,Double,Double) deriving (Eq) 

instance Show ClassifierAccuracy where
  show (ClassifierAccuracy a) = "Classifier Accuracy Report\n" ++
                                "--------------------------\n" ++
                                (printf "%-13s  %10s %10s %10s %10s\n" labelBlank labelValue labelMean labelMedian labelSTDDev ) ++ (concat $ map (\x -> (show x) ++ "\n") a)

instance Show AccuracyResult where
  show (AccuracyResult Precision v (a, b, c)) = (printf "%-13s: %10.3f %10.3f %10.3f %10.3f" labelPrecision v a b c) 
  show (AccuracyResult Recall v (a, b, c)) =    (printf "%-13s: %10.3f %10.3f %10.3f %10.3f" labelRecall v a b c) 
  show (AccuracyResult FMeasure v (a, b, c)) =  (printf "%-13s: %10.3f %10.3f %10.3f %10.3f" labelFMeasure v a b c)
  show (AccuracyResult Informal v (a, b, c)) =  (printf "%-13s: %10.3f %10.3f %10.3f %10.3f" labelInformal v a b c)
  show (AccuracyResult Accuracy v (a, b, c)) =  (printf "%-13s: %10.3f %10.3f %10.3f %10.3f" labelAccuracy v a b c)

classify :: (Show a) => a -> Tree (Decision a) -> Explanation -> (Classification a)
classify example (Node (Decision _ _ _ examples (Just d)) []) path = {-trace ("Judgement:" ++ (show d) ++ "\nPATH:" ++ (show path) ) $ -}  Classification d example path
classify example (Node (Decision _ _ _ _ _) leaves) path = {-trace ("Classifying on: " ++ matchingNodeDescription ) $ -} classify example matchingNode (matchingDescription:path)
  where
    matchingNodeDescription = let (Node (Decision (AttributeClassification _ mnd f) _ _ _ _) _) = matchingNode in mnd
    matchingNode = head $ filter (\(Node (Decision (AttributeClassification _ _ f) _ _ _ _) _) -> f example) leaves
    matchingDescription = let (AttributeClassification _ description _) =  classification $ rootLabel matchingNode in description

classifyInstance ::  (Show a) => Tree (Decision a)      -- the decision tree
                     -> a                   -- the instance to classify
                     -> (Classification a)  -- what we classified this instance as
classifyInstance tree example = classify example tree []


-- score this model in terms of precision, recall, and F measure
-- to do that we need to collect sets of pairings of scores

classifyInstances :: (Show a) =>
  [a]                   -- the instances to check
  -> Tree (Decision a)  -- the classifier
  -> [Classification a]
classifyInstances examples tree = map (\example-> classifyInstance tree example) examples


-- trace ("[TrueClass=" ++ (show $ f example) ++ "] [Guessed=" ++ (show guess) ++ "]\nExample: " ++ (show example)  ) $

scoreClassification :: (Show a) =>
  Classification a        -- something we classified
  -> (a -> Bool) -- a function that determines which class we should be in
  -> ClassificationScore a
scoreClassification c@(Classification guess example _) f =  case (guess, (f example)) of
  (Yes, True) -> ClassificationScore c TruePositive
  (No,  False) -> ClassificationScore c TrueNegative
  (Yes, False) -> ClassificationScore c FalsePositive
  (No,  True) -> ClassificationScore c FalseNegative
  (CantSay d, True) ->   if d >= 0.5 then  ClassificationScore c TruePositive else ClassificationScore c FalseNegative
  (CantSay d, False) ->  if d >= 0.6 then ClassificationScore c FalsePositive else ClassificationScore c TrueNegative

findInformal :: [(ClassificationScore a)] -> Double
findInformal s = (fp+fn)/l 
                 where
                   tp = fromIntegral $ truePositives s
                   tn = fromIntegral $ trueNegatives s
                   fn = fromIntegral $ falseNegatives s
                   fp = fromIntegral $ falsePositives s
                   l  = fromIntegral $ length s

findPrecision :: [(ClassificationScore a)] -> Double
findPrecision s = tp/(tp + fp)
                 where
                   tp = fromIntegral $ truePositives s
                   tn = fromIntegral $ trueNegatives s
                   fn = fromIntegral $ falseNegatives s
                   fp = fromIntegral $ falsePositives s

findAccuracy ::  [(ClassificationScore a)] -> Double
findAccuracy s = (tp+tn)/(tp + tn + fp + fn)
                 where
                   tp = fromIntegral $ truePositives s
                   tn = fromIntegral $ trueNegatives s
                   fn = fromIntegral $ falseNegatives s
                   fp = fromIntegral $ falsePositives s
                   

findRecall :: [(ClassificationScore a)] -> Double
findRecall s = tp/(tp + fn)
                 where
                   tp = fromIntegral $ truePositives s
                   tn = fromIntegral $ trueNegatives s
                   fn = fromIntegral $ falseNegatives s
                   fp = fromIntegral $ falsePositives s

findFMeasure :: [(ClassificationScore a)] -> Double
findFMeasure s = 2 * ((precision*recall)/(precision+recall))
                 where
                   recall = findRecall s
                   precision = findPrecision s
  
truePositives :: [(ClassificationScore a)] -> Int
truePositives scores = length (filter (\(ClassificationScore _ x) -> x==TruePositive) scores)

trueNegatives :: [(ClassificationScore a)] -> Int
trueNegatives scores = length (filter (\(ClassificationScore _ x) -> x==TrueNegative) scores)

falsePositives :: [(ClassificationScore a)] -> Int
falsePositives scores = length (filter (\(ClassificationScore _ x) -> x==FalsePositive) scores)

falseNegatives :: [(ClassificationScore a)] -> Int
falseNegatives scores = length (filter (\(ClassificationScore _ x) -> x==FalseNegative) scores)

accuracy :: [(ClassificationScore a)] -> ClassifierAccuracy
accuracy s = ClassifierAccuracy [ AccuracyResult Precision (findPrecision s) (0,0,0)
                                , AccuracyResult Recall    (findRecall s)    (0,0,0)
                                , AccuracyResult FMeasure  (findFMeasure s)  (0,0,0)
                                , AccuracyResult Accuracy  (findAccuracy s)  (0,0,0)
                                , AccuracyResult Informal  (findInformal s)  (0,0,0)]
 
score :: (Show a) =>
  [a]
  -> Tree (Decision a)
  -> (a -> Bool) -- a function that determines which class we should be in
  -> ClassifierAccuracy
score examples tree f = accuracy $ map (\i -> scoreClassification i f ) (classifyInstances examples tree)


findKinds :: (Show a) =>
  ClassificationClass
  -> [a]
  -> Tree (Decision a)
  -> (a -> Bool) -- a function that determines which class we should be in
  -> [(ClassificationScore a)]
findKinds kind examples tree f = filter (\(ClassificationScore _ x) -> x==kind)  $ map (\i -> scoreClassification i f ) (classifyInstances examples tree)

-- a k-fold validation  
validate :: (LoadableModel a, Eq a, Show a) =>
  [a]                            -- examples
  -> ([a] -> StopCondition)      -- purity function
  -> ([a] -> [AttributeGroup a]) -- the attribute builder
  -> Int                         -- number of sets
  -> (a -> Bool)                 -- category function
  -> IO ([(AccuracyResult,AccuracyResult,AccuracyResult)], ClassifierAccuracy)
validate examples purityFunction decisionTreeAttributes k categoryFunction = do
  examples' <- shuffle examples
  let parts = trace ("Validating with k=" ++ (show k)) (split examples' k)
  -- let perms = permutations parts
  c <- trace ("Validation Set Size=" ++ (show (length (parts !! 0))) ++ ", Training Set Size=" ++ (show ((length examples) - (length (parts !! 0))   ))) $ makeCounter
  scores <- mapM (\validationModel-> do
           let trainingModel = trace ("Validating Model...") $ concat (delete validationModel parts)
           let attributes =  decisionTreeAttributes trainingModel
           tree <- buildTree NoAttribute attributes trainingModel purityFunction 0 c
           return (score validationModel tree categoryFunction)) parts
            
  -- average the scores...
  --   return $ trace (show scores) $ rollup scores
  return $ (seriesData scores, rollup scores)


seriesData :: [ClassifierAccuracy] -> [(AccuracyResult,AccuracyResult, AccuracyResult)]
seriesData scores = map (\x -> ((findType Accuracy x), (findType FMeasure x), (findType Informal x))) scores
                where
                  findType t (ClassifierAccuracy s) = head $ filter (\x -> case (t,x) of
                                         (Precision, (AccuracyResult Precision _ _)) -> True
                                         (Recall,    (AccuracyResult Recall    _ _)) -> True
                                         (FMeasure,  (AccuracyResult FMeasure  _ _)) -> True
                                         (Accuracy,  (AccuracyResult Accuracy  _ _)) -> True
                                         (Informal,  (AccuracyResult Informal  _ _)) -> True
                                         _ -> False) s

 

rollup :: [ClassifierAccuracy] -> ClassifierAccuracy
rollup scores = ClassifierAccuracy [
                                     AccuracyResult Precision (mean p) (mean p, median p, stddev p)   -- precision 
                                   , AccuracyResult Recall (mean r) (mean r, median r, stddev r)      -- recall
                                   , AccuracyResult FMeasure (mean f) (mean f, median f, stddev f)    -- fmeasure
                                   , AccuracyResult Accuracy (mean a) (mean a, median a, stddev a)    -- fmeasure
                                   , AccuracyResult Informal (mean i) (mean i, median i, stddev i) ]  -- informal 
                where
                  p = findScores Precision
                  r = findScores Recall
                  f = findScores FMeasure
                  i = findScores Informal
                  a = findScores Accuracy
                  extractScores = concat $ map (\(ClassifierAccuracy x) -> x) scores
                  findScores t =  map (\(AccuracyResult  t v _) -> v) (findType t)
                  findType t = filter (\x -> case (t,x) of
                                         (Precision, (AccuracyResult Precision _ _)) -> True
                                         (Recall,    (AccuracyResult Recall    _ _)) -> True
                                         (FMeasure,  (AccuracyResult FMeasure  _ _)) -> True
                                         (Informal,  (AccuracyResult Informal  _ _)) -> True
                                         (Accuracy,  (AccuracyResult Accuracy  _ _)) -> True
                                         _ -> False) extractScores



