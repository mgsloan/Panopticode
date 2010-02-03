> {-# OPTIONS -fglasgow-exts #-}
> module Edge where

> import Data.IORef
> import qualified Data.Foldable as F
> import qualified Graphics.UI.GLUT as GL
> import System.Exit
> import qualified Data.List.Zipper as Z

> rightZ, leftZ :: Z.Zipper a -> a
> rightZ (Z.Zip _ (a:_)) = a
> leftZ (Z.Zip (a:_) _) = a

> type Edge a = [a]
> data EdgeZipper a = EdgeZipper { ypos :: Int,  ezip :: (Z.Zipper a) }

> positions :: [a] -> [Int] -> [a]
> positions xs = edgePositions (newEdgeZipper xs)

> newEdgeZipper :: [a] -> EdgeZipper a
> newEdgeZipper = EdgeZipper 0 . Z.fromList

> seek :: Int -> EdgeZipper a -> EdgeZipper a
> seek y ez@(EdgeZipper cy z)
>     | y == cy = EdgeZipper cy z
>     | y >  cy = EdgeZipper (cy-1) (Z.left z)
>     | y <  cy = EdgeZipper (cy+1) (Z.right z)

> edgePositions ez (y:ys) = leftZ z : edgePositions sz ys
>     where sz@(EdgeZipper _ z) = seek y ez