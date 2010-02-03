module Data.Geom2.Classes where

class Scale a where
    scale :: Double -> a -> a

instance Scale Double where
    scale = (*)

class Interpolate a where
    interpolate :: Double -> a -> a -> a

instance Interpolate Double where
    interpolate t a b = a * (1 - t) + b * t