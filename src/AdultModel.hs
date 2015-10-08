{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies,DeriveGeneric, DeriveAnyClass #-}

module AdultModel where

import Model
import DecisionTree
import Data.List
import Data.String.Utils

data AdultModel = AdultModel { age              :: Integer -- 0
                             , fnlwgt           :: Integer -- 2
                             , education        :: String  -- 3
                             , marriageStatus   :: String  -- 5
                             , occupation       :: String  -- 6
                             , relationship     :: String  -- 7
                             , race             :: String  -- 8
                             , capitalGain      :: Integer -- 10
                             , hours            :: Integer -- 12
                             , income           :: String  -- 14
                             }
                deriving (Show, Eq)

instance LoadableModel AdultModel where
  readModel model = map (\row -> toInstance row) model

  categoryFunction example = (\x ->  isInfixOf "<=50K" (income x)) example

  loadModel = readAdultCSVFile >>= \x -> return (readModel x)

  purityFunction examples
    | ((length $ filter categoryFunction examples) == length examples) = Yes
    | ((length $ filter categoryFunction examples) == 0)               = No
    | otherwise = CantSay (fractionYes)
    where
      fractionYes = (fromIntegral $ length (filter categoryFunction examples)) / (fromIntegral $ length examples)

  decisionTreeAttributes ds = [
    -- all the numeric attributes first
      AttributeGroup 0 "Age"         $  (splitAttribute "Age" (toDouble age) (extract (toDouble age)))
    , AttributeGroup 0 "CapitalGain" $  (splitAttribute "CapitalGain" (toDouble capitalGain) (extract (toDouble capitalGain)))
    , AttributeGroup 0 "Hours"       $  (splitAttribute "Hours" (toDouble hours) (extract (toDouble hours)))   
    , educationAttributes
    , AttributeGroup 0 "MarriageStatus" $ splitCategory "MarriageStatus" (marriageStatus) ds
      --, AttributeGroup 0 "Occupation" $ splitCategory "Occupation" (occupation) ds
      -- , AttributeGroup 0 "Relationship" $ splitCategory "Relationship" (relationship) ds
    , AttributeGroup 0 "Race" $ splitCategory "Race" (race) ds
    ]
    where
      extract f = map (\x -> f x) ds
      toDouble f x = fromInteger (f x) :: Double

  toInstance row   = AdultModel 
                     (read (row !! 0)) 
                     (read (row !! 2))
                     (strip (row !! 3))
                     (strip (row !! 5))
                     (strip (row !! 6))
                     (strip (row !! 7))
                     (strip (row !! 8))
                     (read (row !! 10))
                     (read (row !! 12))
                     (strip (row !! 14))

--
-- Support Functions
--


splitCategory ::
  String -- category name
  -> (AdultModel -> String)
  -> [AdultModel]
  -> [Attribute AdultModel]
splitCategory name category ds = map (\maker -> AttributeClassification name (name ++ "=" ++ maker) (\x -> category x == maker)) (extract)
  where
    extract = nub $ map (\x -> category x) ds :: [String]

educationAttributes  = AttributeGroup 0 "Education" [
  AttributeClassification "Education" "More than HS" (\x -> elem (education x) ["Bachelors", "Some-college", "Prof-school", "Assoc-acdm", "Assoc-voc","Masters", "Doctorate"] )
  , AttributeClassification "Education" "High School or Less" (\x -> elem (education x) ["11th", "9th", "7th-8th", "HS-grad", "12th", "1st-4th", "10th", "5th-6th", "Preschool"] )
  ]

readAdultCSVFile :: IO [[String]]
readAdultCSVFile = readCSVFile "data/adult/adult.data.txt"

