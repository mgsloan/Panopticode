module Data.Geom2.Interval where
import Data.Geom2.Classes

data (Ord a) => Interval a = Interval a a
    deriving (Eq, Ord, Bounded)

from (Interval f _) = f
to (Interval _ t) = t

instance (Show a, Num a, Ord a) => Show (Interval a) where
    show (Interval f t) = "I(" ++ (show f) ++ ", " ++ (show t) ++ ")"

zipInterval :: (Ord a, Ord b, Ord c) => (a -> b -> c) -> Interval a -> Interval b -> Interval c 
zipInterval f (Interval ax ay) (Interval bx by) = Interval (f ax bx) (f ay by)

newInterval :: (Ord a) => a -> a -> Interval a
newInterval f t | f > t = Interval t f
newInterval f t = Interval f t

maybeInterval :: (Ord a) => a -> a -> Maybe (Interval a)
maybeInterval f t | f > t = Nothing
maybeInterval f t = Just $ Interval f t

intervalContains (Interval f t) v = v >= f && v <= t

class Set a where
    union :: a -> a -> a
    intersect :: a -> a -> Maybe a

instance (Ord a) => Set (Interval a) where
    union (Interval f1 t1) (Interval f2 t2) = Interval (min f1 f2) (max t1 t2)
    intersect (Interval f1 t1) (Interval f2 t2) = maybeInterval (max f1 f2) (min t1 t2)

instance (Num a, Ord a) => Num (Interval a) where
    (+) = zipInterval (+)
    (-) = zipInterval (-)
    (Interval f1 t1) * (Interval f2 t2) = newInterval (f1 * f2) (t1 * t2) `union` newInterval (f1 * t2) (t1 * f2)
    abs i@(Interval f t) | i `intervalContains` 0 = Interval 0 (max (abs f) (abs t))
                         | otherwise = newInterval (abs f) (abs t)
    signum (Interval f t) = newInterval (signum f) (signum t)
    negate (Interval f t) = Interval (-t) (-f)
    fromInteger i = Interval (fromInteger i) (fromInteger i)

instance (Scale a, Ord a) => Scale (Interval a) where
    scale v i = newInterval (scale v $ from i) (scale v $ to i)

extent, middle :: (Fractional a, Ord a) => Interval a -> a
extent (Interval f t) = t - f
middle (Interval f t) = (f + t) / 2

extend :: (Ord a) => Interval a -> a -> Interval a
extend (Interval f t) v | v < f = Interval v t
                        | v > t = Interval f v
                        | otherwise = Interval f t

expand :: (Fractional a, Ord a) => Interval a -> a -> Interval a
expand i@(Interval f t) v | v + (extent i) >= 0 = Interval (f - v) (t + v)
                          | otherwise = Interval (middle i) (middle i)

intervalMapping :: (Fractional a, Ord a) => Interval a -> Interval a -> a -> a
intervalMapping fr@(Interval f1 _) to@(Interval f2 _) a = (a - f1) * (extent to / extent fr) + f2

wrapIntoInterval :: (RealFrac a, Ord a) => Interval a -> a -> a
wrapIntoInterval i = intervalMapping (Interval 0 1) i . (\x -> (if x < 0 then (1+) else id) $ snd (properFraction x)) . intervalMapping i (Interval 0 1)