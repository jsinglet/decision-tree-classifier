module Minimize where

mean :: (Fractional a, Foldable t) => t a -> a
mean d = (sum d)/(fromIntegral (length d))

data MinimizationResult a = MinimizationResult (a, a) -- MinimizationResult (whereToSplit, minimizationResult)
                          | NoResult                  -- For the initial minimization
                          deriving (Show)

minimize ::  (Fractional a, Ord a) => [a] -> MinimizationResult a -> [a] -> MinimizationResult a
minimize d res (s:ss) = case (res) of             NoResult -> minimize d (MinimizationResult (s, sigmaTerm)) ss
                                                  MinimizationResult (_, splitVal) -> if sigmaTerm < splitVal then minimize d (MinimizationResult (s, sigmaTerm)) ss else minimize d res ss
  where
      sigmaTerm = sum t1 + sum t2
      r1 = [ x | x <- d, x <= s]
      r2 = [ x | x <- d, x >  s]
      b1 = mean r1
      b2 = mean r2
      -- the left and right term
      t1 = [ (x - b1)*(x - b1) | x <- r1]
      t2 = [ (x - b2)*(x - b2) | x <- r2]

minimize d res [] = res

