-- | Some statistical functions (imported from the hstats package)

module Stats where

import Data.List
import Data.Ord (comparing)

-- |Numerically stable mean
mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

-- |Same as 'mean' 
average :: Floating a => [a] -> a
average = mean

-- |Harmonic mean
harmean :: (Floating a) => [a] -> a
harmean xs = fromIntegral (length xs) / (sum $ map (1/) xs)

-- |Geometric mean
geomean :: (Floating a) => [a] -> a
geomean xs = (foldr1 (*) xs)**(1 / fromIntegral (length xs))

-- |Median
median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

-- |Modes returns a sorted list of modes in descending order
modes :: (Ord a) => [a] -> [(Int, a)]
modes xs = sortBy (comparing $ negate.fst) $ map (\x->(length x, head x)) $ (group.sort) xs

-- |Mode returns the mode of the list, otherwise Nothing
mode :: (Ord a) => [a] -> Maybe a
mode xs = case m of
            [] -> Nothing
            otherwise -> Just . snd $ head m
    where m = filter (\(a,b) -> a > 1) (modes xs)

-- |Central moments
centralMoment :: (Floating b, Integral t) => [b] -> t -> b
centralMoment xs 1 = 0
centralMoment xs r = (sum (map (\x -> (x-m)^r) xs)) / n
    where
      m = mean xs
      n = fromIntegral $ length xs

-- |Range
range :: (Num a, Ord a) => [a] -> a
range xs = maximum xs - minimum xs

-- |Average deviation
avgdev :: (Floating a) => [a] -> a
avgdev xs = mean $ map (\x -> abs(x - m)) xs
    where
      m = mean xs

-- |Standard deviation of sample
stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ var xs

-- |Sample variance
var xs = (var' 0 0 0 xs) / (fromIntegral $ length xs - 1)
    where
      var' _ _ s [] = s
      var' m n s (x:xs) = var' nm (n + 1) (s + delta * (x - nm)) xs
         where
           delta = x - m
           nm = m + delta/(fromIntegral $ n + 1)
