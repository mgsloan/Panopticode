module Subpixel where

import Data.Geom2.D2
import Data.List (tranpose)

subpixelLine :: [GLubyte] -> [Glubyte]
subpixelLine xs = map sum . transpose $ [(++[0,0]).tail.tail $ as, tail bs ++ [0], zipWith (+) as bs, 0:init bs, (0:0:).init.init $ as)]
  where (as,bs) = mapT (\c -> map ((*c).fromIntegral) xs) (1.0/9, 2.0/9)