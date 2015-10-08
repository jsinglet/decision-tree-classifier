{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, TypeFamilies #-}

module MPGModel where

import Model
import DecisionTree
import Data.List
import Debug.Trace

--
-- The model
-- 
data MPGModel = MPGModel { mpg          :: Double
                         , cylinders    :: Integer
                         , displacement :: Double
                         , horsepower   :: Double
                         , weight       :: Integer
                         , acceleration :: Double
                         , modelYear    :: Integer
                         , origin       :: Integer
                         , carName      :: String
                         }
              deriving (Show, Eq)

--
-- Class instances for modeling
--
instance LoadableModel MPGModel where
  categoryFunction example = (\x -> mpg x > 20) example
  purityFunction examples
    | ((length $ filter categoryFunction examples) == length examples) = Yes
    | ((length $ filter categoryFunction examples) == 0)               = No
    | otherwise = CantSay (fractionYes)
    where
      fractionYes = (fromIntegral $ length (filter categoryFunction examples)) / (fromIntegral $ length examples)

  readModel model = map (\row -> toInstance row) model
  toInstance row   = MPGModel 
                     (read (row !! 1)) 
                     (read (row !! 2))
                     (read (row !! 3))
                     (read (row !! 4))
                     (read (row !! 5))
                     (read (row !! 6)) 
                     (read (row !! 7))
                     (read (row !! 8))
                     (row !! 9)
  loadModel = readAutoMPGCSVFile >>= \x -> return (readModel x)
  decisionTreeAttributes ds = [ AttributeGroup 0 "Cylinders"    $ (splitAttribute "Cylinders" (toDouble cylinders) (extract (toDouble cylinders)))
                              , AttributeGroup 0 "Displacement" $ (splitAttribute "Displacement" displacement (extract displacement))
                              , AttributeGroup 0 "Horsepower"   $ (splitAttribute "Horsepower" horsepower (extract horsepower))
                              , AttributeGroup 0 "Weight"       $ (splitAttribute "Weight" (toDouble weight) (extract (toDouble weight)))
                              , AttributeGroup 0 "Acceleration" $ (splitAttribute "Acceleration" acceleration (extract acceleration))
                              , AttributeGroup 0 "ModelYear"    $ (splitAttribute "ModelYear" (toDouble modelYear) (extract (toDouble modelYear)))
                              , AttributeGroup 0 "Origin"       $ (splitAttribute "Origin" (toDouble origin) (extract (toDouble origin)))
                              , AttributeGroup 0 "CarName"      $
                                map (\maker -> AttributeClassification "CarName" ("CarName=" ++ maker) (\x -> elem maker (words (carName x)))) carMakers
                                ]
    where
      extract f = map (\x -> f x) ds
      toDouble f x = fromInteger (f x) :: Double


-- utility function 
allCarMakers :: [MPGModel] -> [String]
allCarMakers model = nub $ map (\x -> head $ words (carName x)) model

carMakers = ["chevrolet","buick","plymouth","amc","ford","pontiac","dodge","toyota","datsun","volkswagen","peugeot","audi","saab","bmw","chevy","hi","mercury","opel","fiat","oldsmobile","chrysler","mazda","volvo","renault","toyouta","maxda","honda","subaru","chevroelt","capri","vw","mercedes-benz","cadillac","mercedes","vokswagen","triumph","nissan"]
                  
readAutoMPGCSVFile :: IO [[String]]
readAutoMPGCSVFile = readCSVFile "data/auto-mpg/auto-mpg.data"

