> module Optimizer where

> import State
> import Data.List (findIndices)
> import Data.Geom2.D2

> data Move = ChangePos { changeName :: String, changePos :: (Int, Int) }

> type Optimizer = Rectangle -> [Buffer] -> ([Buffer], [Move])

Assumes that the first buffer is preferred

optimize :: Optimizer
optimize = (\x -> (x, contourMatch x)) . fillShapeCache
    where contourMatch = map (\v -> map (match v) rights) lefts
          (lefts, rights) = foldl (\b (ls,rs) -> let (Channel l r) = fromJust . shapeCache $ b in (l:ls,r:rs)) ([],[])
          match = (wastedSpace, offset)


> query :: String -> [Buffer] -> [(Int, Int)]
> query str = concatMap (\(i, b) -> map (\x->(i,x)) . findIndices $ bufferText b) . zip [0..]
