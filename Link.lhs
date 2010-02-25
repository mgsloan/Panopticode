> {-# OPTIONS -fglasgow-exts #-}
> module Link where
> import State
> import Data.Geom2.D2
> import qualified Data.Set as Set
> import qualified Data.Map as Map
> import qualified Data.List as List
> import Data.Ord

> class Annotation a where
>     render :: a -> IO ()
>     participantWeights :: a -> Map.Map String Double

> class (Annotation a) => LinkAnnotation a b | a -> b where
>     label :: a -> b
>     textPositions :: a -> (Int, Int)
>     visualPositions :: a -> ((Int, Int), (Int, Int))

> data (LinkAnnotation a b) => SuperNode a b = SuperNode [a]

instance (LinkAnnotation a b) => Annotation (SuperNode a) where
    render =

> data (Annotation a) => AnnotationSet a = AnnotationSet { aset :: Set.Set a }

> instance (Annotation a) => Annotation (AnnotationSet a) where
>     render = mapM_ (render) . Set.elems . aset   --TODO: bars which indicate relation between links
>     participantWeights = Map.unionsWith (+) . map participantWeights . Set.elems . aset

> average :: (Fractional a) => [a] -> a
> average xs = (/(fromIntegral $ length xs)) $ sum xs

> averageVisualPositions :: (LinkAnnotation a b) => AnnotationSet a -> ((Double, Double), (Double, Double))
> averageVisualPositions = average . map (mapT (mapT fromIntegral) . visualPositions) . Set.elems . aset

> labels :: (LinkAnnotation a b, Ord a, Ord b) => AnnotationSet a -> Set.Set b
> labels = Set.map label . aset

> maxWeight :: (Annotation a) => a -> (String, Double)
> maxWeight = List.maximumBy (comparing snd) . Map.toList . participantWeights
