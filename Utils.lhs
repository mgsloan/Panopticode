> module Utils where
> import Data.Geom2.D2
> import qualified Graphics.UI.GLUT as GL
> import Debug.Trace

> log2 :: (Floating a) => a -> a
> log2 x = log x / log 2.0
> log2i :: (Integral b, Floating a) => b -> a
> log2i = log2 . fromIntegral

> padRight, padLeft :: Int -> a -> [a] -> [a]
> padRight i x xs = xs ++ (take (i - length xs) $ repeat x)
> padLeft i x xs = (take (i - length xs) $ repeat x) ++ xs

> mutateList :: Int -> a -> a -> [a] -> [a]
> mutateList ix d x xs = (padRight ix d $ take ix xs) ++ (x:drop (ix+1) xs)

> checkList :: a -> Int -> [a] -> a
> checkList _ 0 (x:xs) = x
> checkList d 0 [] = d
> checkList d ix xs = checkList d (ix-1) $ tail xs

> bothFromI :: (Integral a, Num b) => (a,a) -> (b,b)
> bothFromI = mapT fromIntegral

> traceIt :: (Show a) => a -> a
> traceIt x = trace (show x) x

> breakEvery _ [] = []
> breakEvery n xs = cur : (breakEvery n rest)
>    where (cur, rest) = splitAt n xs

> infixr 8 ><
> (><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
> (f >< g) (x,y) = (f x,g y)