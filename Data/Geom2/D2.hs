{-# OPTIONS_GHC -fglasgow-exts #-} 

module Data.Geom2.D2 where
import Data.Geom2.Interval
import Data.Geom2.Classes

-- taken  from Data.Graph.Inductive.Query.Monad
infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f = f >< f

zipT :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipT f (ax,ay) = f ax >< f ay

foldT :: (a -> a -> b) -> (a, a) -> b
foldT f (x, y) = f x y

flipT :: (a,b) -> (b,a)
flipT (a,b) = (b,a)

vx, vy :: Num a => (a, a) -> a
vx = fst; vy = snd

dot, cross :: Num a => (a,a) -> (a, a) -> a
dot a b = uncurry (+) (a*b)
cross a b = dot a (cw b)

angle :: RealFloat a => (a, a) -> a
angle (x, y) = atan2 y x

distance, distanceSq :: Floating a => (a, a) -> a
distance = sqrt . distanceSq
distanceSq (x, y) = x*x + y*y

pnorm :: Floating a => Integer -> (a, a) -> a 
pnorm 1 = uncurry (+)
pnorm 2 = distance
pnorm p = (**(recip $ fromInteger p)) . uncurry (+) . mapT (^p)

infinityNorm :: Ord a => (a, a) -> a
infinityNorm = uncurry (max)

normalize :: (Double, Double) -> (Double, Double)
normalize (0, 0) = (0, 0)
normalize v = scale (recip . distance $ v) v

cw, ccw :: Num a => (a, a) -> (a, a)
cw (x, y)  = (y, -x)
ccw (x, y) = (-y, x)

d2both, d2one :: Num a => (a -> Bool) -> (a, a) -> Bool
d2both f (a,b) = f a && f b
d2one f (a,b) = f a || f b

instance Num a => Num (a, a) where
    (+) = zipT (+)
    (-) = zipT (-)
    (*) = zipT (*)
    abs = mapT abs
    signum = mapT signum
    negate = mapT negate
    fromInteger i = (fromInteger i, fromInteger i)

instance Fractional a => Fractional (a, a) where
    (/) = zipT (/)
    recip = mapT recip
    fromRational = const undefined

instance Scale a => Scale (a, a) where
    scale v = mapT (scale v)

instance Interpolate a => Interpolate (a, a) where
    interpolate t = zipT (interpolate t)

type Vector = (Double, Double)
type Rectangle = (Interval Double, Interval Double)

nearestSegPoint :: Vector -> (Vector, Vector) -> Vector
nearestSegPoint p (a, b) =
  if da <= 0 then a else
  if db <= 0 then b else a + scale (recip (da + db)) (scale da (b - a)) 
 where da = dot (p - a) (b - a)
       db = dot (p - b) (a - b)