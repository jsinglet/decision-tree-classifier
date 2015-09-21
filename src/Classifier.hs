module Classifier where

import Data.Tree
import DecisionTree hiding (classify)
import Debug.Trace
import Model
import Data.List
import Stats
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
  deriving (Show,Eq)

data AccuracyResult = AccuracyResult AccuracyMeasure Double (Double,Double,Double) deriving (Eq) 

instance Show ClassifierAccuracy where
  show (ClassifierAccuracy a) = "Classifier Accuracy Report\n" ++ "--------------------------\t(mean/median/stddev)\n" ++ (concat $ map (\x -> (show x) ++ "\n") a)

instance Show AccuracyResult where
  show (AccuracyResult Precision v (a, b, c)) = "Precision: (" ++ (show v) ++ ")  " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)
  show (AccuracyResult Recall v (a, b, c)) =    "Recall:    (" ++ (show v) ++ ")  " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)
  show (AccuracyResult FMeasure v (a, b, c)) =  "FMeasure:  (" ++ (show v) ++ ")  " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)
  show (AccuracyResult Informal v (a, b, c)) =  "Informal:  (" ++ (show v) ++ ")  " ++ (show a) ++ "," ++ (show b) ++ "," ++ (show c)

classify :: a -> Tree (Decision a) -> Explanation -> (Classification a)
classify example (Node (Decision _ _ _ examples (Just d)) _) path = Classification d example path
classify example (Node (Decision _ _ _ _ _) leaves) path = classify example matchingNode (matchingDescription:path)
  where
    matchingNode = head $ filter (\(Node (Decision (AttributeClassification _ _ f) _ _ _ _) _) -> f example) leaves
    matchingDescription = let (AttributeClassification _ description _) =  classification $ rootLabel matchingNode in description

classifyInstance ::  Tree (Decision a)      -- the decision tree
                     -> a                   -- the instance to classify
                     -> (Classification a)  -- what we classified this instance as
classifyInstance tree example = classify example tree []


-- score this model in terms of precision, recall, and F measure
-- to do that we need to collect sets of pairings of scores

classifyInstances ::
  [a]                   -- the instances to check
  -> Tree (Decision a)  -- the classifier
  -> [Classification a]
classifyInstances examples tree = map (\example-> classifyInstance tree example) examples

scoreClassification ::
  Classification a        -- something we classified
  -> (a -> Bool) -- a function that determines which class we should be in
  -> ClassificationScore a
scoreClassification c@(Classification guess example _) f =  case (guess, (f example)) of
  (Yes, True) -> ClassificationScore c TruePositive
  (No,  False) -> ClassificationScore c TrueNegative
  (Yes, False) -> ClassificationScore c FalsePositive
  (No,  True) -> ClassificationScore c FalseNegative
  (CantSay d, True) -> if d >= 0.5 then  ClassificationScore c TruePositive else ClassificationScore c FalseNegative
  (CantSay d, False) -> if d >= 0.6 then  ClassificationScore c FalsePositive else ClassificationScore c TrueNegative

findInformal :: [(ClassificationScore a)] -> Double
findInformal s = (fromIntegral $ truePositives s + trueNegatives s)/(fromIntegral $ length s)

findPrecision :: [(ClassificationScore a)] -> Double
findPrecision s = (fromIntegral $ truePositives s)/(fromIntegral $ (truePositives s + falsePositives s))

findRecall :: [(ClassificationScore a)] -> Double
findRecall s = (fromIntegral $ truePositives s)/(fromIntegral $ (truePositives s + falseNegatives s))

findFMeasure :: [(ClassificationScore a)] -> Double
findFMeasure s = 2 * ( ((findPrecision s) * (findRecall s))/((findPrecision s)+(findRecall s)))
  
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
                                , AccuracyResult Informal  (findInformal s)  (0,0,0)]
 
score ::
  [a]
  -> Tree (Decision a)
  -> (a -> Bool) -- a function that determines which class we should be in
  -> ClassifierAccuracy
score examples tree f = accuracy $ map (\i -> scoreClassification i f ) (classifyInstances examples tree)


findKinds ::
  ClassificationClass
  -> [a]
  -> Tree (Decision a)
  -> (a -> Bool) -- a function that determines which class we should be in
  -> [(ClassificationScore a)]
findKinds kind examples tree f = filter (\(ClassificationScore _ x) -> x==kind)  $ map (\i -> scoreClassification i f ) (classifyInstances examples tree)

-- a k-fold validation 
validate :: (LoadableModel a) =>
  [a]                            -- examples
  -> ([a] -> StopCondition)      -- purity function
  -> ([a] -> [AttributeGroup a]) -- the attribute builder
  -> Int                         -- number of sets
  -> (a -> Bool)                 -- category function
  -> IO ClassifierAccuracy
validate examples purityFunction decisionTreeAttributes k categoryFunction = do
  examples' <- shuffle examples
  let parts = trace ("Validating with k=" ++ (show k)) (split examples' k)
  let perms = permutations parts
  c <- makeCounter
  scores <- mapM (\(validationModel:trainingSets)-> do
           let trainingModel = trace ("Validating Model...") $ concat trainingSets
           let attributes = decisionTreeAttributes trainingModel
           tree <- buildTree NoAttribute attributes trainingModel purityFunction 0 c
           return (score validationModel tree categoryFunction)) perms
            
  -- average the scores...
  --   return $ trace (show scores) $ rollup scores
  return $ rollup scores


 

rollup :: [ClassifierAccuracy] -> ClassifierAccuracy
rollup scores = ClassifierAccuracy [
                                     AccuracyResult Precision (mean p) (mean p, median p, stddev p)   -- precision 
                                   , AccuracyResult Recall (mean r) (mean r, median r, stddev r)   -- recall
                                   , AccuracyResult FMeasure (mean f) (mean f, median f, stddev f)   -- fmeasure
                                   , AccuracyResult Informal (mean i) (mean i, median i, stddev i) ] -- informal 
                where
                  p = findScores Precision
                  r = findScores Recall
                  f = findScores FMeasure
                  i = findScores Informal
                  extractScores = concat $ map (\(ClassifierAccuracy x) -> x) scores
                  findScores t =  map (\(AccuracyResult  t v _) -> v) (findType t)
                  findType t = filter (\x -> case (t,x) of
                                         (Precision, (AccuracyResult Precision _ _)) -> True
                                         (Recall,    (AccuracyResult Recall    _ _)) -> True
                                         (FMeasure,  (AccuracyResult FMeasure  _ _)) -> True
                                         (Informal,  (AccuracyResult Informal  _ _)) -> True
                                         _ -> False) extractScores



