> module Font where
> 
> import Data.Bits
> import Data.Char (ord, chr)
> import Data.List (minimumBy, transpose)
> import Data.Ord  (comparing)
> import Data.Maybe (fromJust)
> import qualified Data.Map as M
> import Control.Arrow (first, second)
> import Graphics.Rendering.OpenGL    
> import Foreign.Marshal.Array
> import Debug.Trace
> import GHC.Ptr

> import Utils
> import Data.Geom2.D2

> data MonoFont = MonoFont { fntCharsz :: (Int, Int), fntTex :: (Maybe TextureObject), fntDat :: (M.Map (Int, Char) [GLubyte]) }

> subpixelLine :: [GLubyte] -> [GLubyte]
> subpixelLine xs = map floor . map sum . transpose $ [(++[0,0]).tail.tail $ as, tail bs ++ [0], zipWith (+) as bs, 0:init bs, (0:).(0:).init.init $ as]
>  where (as,bs) = mapT (\c -> map ((*c).fromIntegral) xs) (1.0/9, 2.0/9)

> stringFont :: Char -> Int -> [[String]] -> MonoFont
> stringFont ch wid xs = MonoFont charsz Nothing $ M.fromList (zip indices chunks)
>  where chunks = breakEvery 3 . map (\x -> if x == ch then 255 else 0) . concat . concat $ xs
>        indices = concatMap (concat . zipWith (\ix -> map (\x -> (ix,x))) [0..] . replicate (snd charsz)) $ breakEvery (fst linesz) [' '..]
>        (linesz, charsz) = let (a@(b:_):_) = xs in ((length b `div` wid, length xs), (wid, length a))

> ceilPow = mapT ((2^) . ceiling . log2i)

> replace x y = concatMap (\z -> if z == x then y else [z])

> data TextRenderContext = TextRenderContext {txtFont :: MonoFont, txtMaxWidth, tabSize :: Int, wrapString :: String}

> initTail :: [a] -> ([a], a)
> initTail [x]    =  ([], x)
> initTail (x:xs) =  (x : ys, v) where (ys,v) = initTail xs
> initTail []     =  error "initTail: empty list"

> processContext :: TextRenderContext -> String -> (Int, [String])
> processContext (TextRenderContext (MonoFont charsz Nothing tex) maxWidth tabSize wrapString) str = (width, wrappedLines)
>  where ls = lines . replace '\t' (replicate tabSize ' ') $ str
>        width = maximum . (if maxWidth == -1 then id else (maxWidth:)) $ map length ls
>        wrappedLines :: [String]
>        wrappedLines = concatMap (wrapLine . breakEvery (width-1)) ls
>            where wrapLine :: [String] -> [String]
>                  wrapLine [] = []
>                  wrapLine xs = (\(a,b) -> a++[b]) . first (map (++wrapString)) . initTail $ xs

> drawText :: MonoFont -> (Int, [String]) -> IO TextureObject
> drawText (MonoFont charsz _ tex) (width, wrappedLines) = do
>      [texName] <- genObjectNames 1
>      textureBinding Texture2D $= Just texName
>      textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
>      newArray dat >>= texImage2D Nothing NoProxy 0 Luminance' tsz2d 0 . PixelData Luminance UnsignedByte
>      textureBinding Texture2D $= Nothing
>      return texName
>  where tsz = ceilPow $ charsz * (width, length wrappedLines)
>        tsz2d = uncurry TextureSize2D . bothFromI $ tsz
>        dat :: [GLubyte]
>        dat = concat . padRight (snd tsz) (replicate (fst tsz) 0) $
>              concatMap (\line -> map (padRight (fst tsz) 0) $ concatMap (\ix -> map (lookup ix) line) [0..snd charsz-1]) wrappedLines
>            where lookup ix x = maybe ([0,0,0]) id $ M.lookup (ix, x) tex

> drawFont :: MonoFont -> IO TextureObject
> drawFont mf = drawText mf . processContext (TextRenderContext mf (-1) 0 "") $ [minimum chars..maximum chars]
>   where chars = map (snd.fst) . M.toList $ fntDat mf

> corner dir = case dir of 0 -> (0,0); 1 -> (1,0); 2 -> (1,1); 3 -> (0,1)

> monouvs :: MonoFont -> (Int,Int) -> Int -> Char -> (GLfloat, GLfloat)
> monouvs (MonoFont charsz _ m) imgsz corn ch | chix >= 0 && chix < foldT (*) linesz = zipT (/) pos . mapT fromIntegral $ imgsz
>     where pos = bothFromI $ charsz * (corner corn + flipT (chix `divMod` lineWidth))
>           chix = ord ch - 32
>           linesz@(lineWidth, _) = mapT floor $ zipT (/) (bothFromI imgsz) (bothFromI charsz)