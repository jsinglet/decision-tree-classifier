{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies, QuasiQuotes #-}

module DecisionTree where

import Data.Tree
import Data.IORef
import Minimize
import Debug.Trace
import Stats
-- 
-- Data types 
-- 
data Decision a = Decision { classification :: Attribute a
                           , parent         :: Maybe (Tree a)
                           , attribute      :: Maybe (AttributeGroup a)
                           , examples       :: [a]
                           , decision       :: Maybe StopCondition
                           } deriving (Show)


data AttributeGroup a = AttributeGroup Int String [Attribute a] 
data Attribute a = AttributeClassification String String (a -> Bool)
                 | NoAttribute 

data StopCondition = Yes | No | CantSay Double deriving (Show)

type Counter = Int -> IO Int

--
-- Class instances
--
instance Show (Attribute a)  where
  show (AttributeClassification name description _) = "{Attribute: " ++ name ++ ", Description: " ++ description ++ "}"
  show NoAttribute = "N/A"

instance Show (AttributeGroup a) where
  show (AttributeGroup generation name attributes) = "AttributeGroup " ++ (show generation) ++ ", " ++ name ++ " [\n" ++ (show attributes) ++ "\n]"


--
-- Traversal functions
--


classify :: AttributeGroup a -> [a] -> [(Attribute a, [a])]
classify (AttributeGroup _ _ attributes)  examples = map (\attribute@(AttributeClassification _ _ f) -> (attribute, filter f examples)) attributes

-- we select the attribute that would produce the lowest gini index. 
selectAttribute ::
  [AttributeGroup a]
  -> [a]
  -> Int
  -> ([a] -> StopCondition) -- the purity function (determines if we can stop or not)
  -> ([AttributeGroup a], AttributeGroup a)
selectAttribute attrs examples d pf = do
  let indexed = map (\x -> ((calculateGiniIndex  (classify x examples) examples pf), x) ) attrs
  let (lowestGiniIndex, a) = foldr (\acc x -> if fst x < fst acc then x else acc) (head indexed) indexed
  let remainingAttributes = filter (\x@(AttributeGroup _ name _)  -> let (AttributeGroup _ name2 _) = a in name /= name2) attrs
  (remainingAttributes, a)
  --trace ("Selected GINI Index: " ++ (show lowestGiniIndex)) $ (remainingAttributes, a)

-- calculateGini classified examples = 1.0 - sum(map (\(_, classifiedExamples) -> ((fromIntegral $ length classifiedExamples)/numExamples)^2) classified)
--                                     where
--                                       numExamples = fromIntegral $ length examples


calculateGiniIndex classified examples pf =
  weightedAverage $
  map (\(_, classifiedExamples)
       ->  (fromIntegral $
            length classifiedExamples) * (1 - ((c1 classifiedExamples)^2  +  (c2 classifiedExamples)^2)) -- Sigma
      ) classified
  where
    c1 ex = case (pf ex) of
      Yes -> 1.0
      No  -> 0.0
      CantSay d -> d
    c2 ex = case (pf ex) of
      Yes -> 0.0
      No  -> 1.0
      CantSay d -> 1.0-d
    weightedAverage x = Stats.mean x
                                           

                                      

buildTree ::
  Attribute a               -- the attribute that decides node
  -> [AttributeGroup a]     -- the attributes
  -> [a]                    -- the training examples
  -> ([a] -> StopCondition) -- the purity function (determines if we can stop or not)
  -> Int                    -- the depth
  -> Counter                -- a counter that is used for making unique nodes
  -> IO (Tree (Decision a))
buildTree attribute _ examples pf 20 c =  return $ Node (Decision attribute Nothing Nothing examples (Just (pf examples))) []
buildTree attribute [] examples pf _ _ =  return $ Node (Decision attribute Nothing Nothing examples (Just (pf examples))) []
buildTree attribute attributes examples pf depth c =  
  -- see if we should stop classifying
  case (pf examples) of
    cs@(CantSay d) -> do
      -- select an attribute to classify
      let (remainingAttributes, AttributeGroup _ name groupAttributes)  = selectAttribute attributes examples depth pf

      nextId <- c 1
      let selectedAttribute = AttributeGroup nextId name groupAttributes
      -- classify on the attribute
      let classifiedExamples = classify selectedAttribute examples
      nextLevel <- mapM (\(attr,attrExamples) -> buildTree attr remainingAttributes attrExamples pf (depth+1) c) classifiedExamples
      return $ Node (Decision attribute Nothing (Just selectedAttribute) examples (Just cs)) nextLevel
    Yes     -> return $ Node (Decision attribute Nothing Nothing examples (Just Yes)) []
    No     -> return $ Node (Decision attribute Nothing Nothing examples (Just No)) []


setFromRange :: (Num a, Ord a, Fractional a) => [a] -> [a]
setFromRange d = step (minimum d) (maximum d) (minimum d)
  where
    step initial end this = if this >= end then [] else  this : (step initial end (this + stepSize))
    stepSize = if (maximum d) <= 100 then 1 else  (maximum d) /  50 


splitAttribute ::
  (Ord b, Fractional b, Show b) => String
  -> (a -> b)
  -> [b]
  -> [Attribute a]
splitAttribute groupName valueFunction ds = let (MinimizationResult (splitPoint, _)) =  minimize ds NoResult (setFromRange ds) in createAttributeFunctions groupName valueFunction splitPoint

createAttributeFunctions :: (Ord b, Show b) => String -> (a -> b) -> b -> [Attribute a]
createAttributeFunctions groupName valueFunction splitPoint =   [
    AttributeClassification groupName (groupName ++ " <= " ++ (show splitPoint)) (\x -> (valueFunction x) <= splitPoint)
  , AttributeClassification groupName (groupName ++ " > " ++ (show splitPoint)) (\x -> (valueFunction x) > splitPoint)
  ]

makeCounter :: IO Counter
makeCounter = do
      r <- newIORef 0
      return (\i -> do modifyIORef r (+i)
                       readIORef r)
