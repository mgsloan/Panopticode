> module Font where
> 
> import Data.Bits
> import Data.Char (ord, chr)
> import Data.List (minimumBy, transpose)
> import Data.Ord  (comparing)
> import Data.Maybe (fromJust)
> import qualified Data.Map as M
> import Control.Arrow (first, second)
> import qualified Graphics.UI.GLUT as GL
> import Graphics.Rendering.OpenGL.GL
> import Foreign.Marshal.Array
> import Debug.Trace
> import GHC.Ptr

> import TinyFont
> import Utils
> import Data.Geom2.D2

> {- tinyFontData = concatMap ((\x -> map (testBit x) [5,4..0]).(subtract 32).ord) $
>        "__]M]2_Z\"\"H,#UJC-%O_K;MVUHO]HM_]?X___>UKJ2M-HJC!QQ2&P8P#\" VV\"\" 0V]]_OKU=W''=U=U]H M02*\",+,*2(88#+8:0"++
>        "2\"0MH&R#1*;;\"\"20 020\"#80V\"*08P%MJ2020J0\"0H20M&K!;9;MTVTJ___XN_\\P0;\"'X8V\"'J<U%M0Q;*5]MWR#:*MMWZ\"_"++
>        "*7X0J+=4V[*?+IM%GZ0_2J0\"_524PXU$KLMMINIZOX"



> writeTinyFont :: String
> writeTinyFont = show $ map transpose . breakEvery 8 . breakEvery 5 . breakEvery 3 . map fromEnum $ tinyFontData

> chars width height = breakEvery (width*height)

> triple f (a,b,c) = (f a, f b, f c)

> fontData :: [[[Bool]]] -> IO Font
> fontData xs = do
>      [texName] <- genObjectNames 1
>      textureBinding Texture2D $= Just texName
>      textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
>      generateMipmap Texture2D $= Enabled
>      arr <- mallocArray (foldT (*) tsz * 3)
>      pokeArray arr dat
>      GL.texImage2D Nothing GL.NoProxy 0 GL.RGB16 tsz2d 0 $ GL.PixelData GL.RGB GL.UnsignedShort $ arr
>      textureBinding Texture2D $= Nothing
>      return $ Font tsz counts sz texName
>    where dat = concatMap (\b -> if b then [128,128,255] else [0,0,0]) $ processed :: [GL.GLushort]
>          processed = concat . padRight height (replicate width False) . concat $ ls
>          ls = map (map (padRight width False) . foldr1 (zipWith (++))) $ breakEvery (fst counts) xs
>          (count, sz@(width, height)) = let (a@(b:_):_) = xs in (length xs, (length b, length a))
>          (logcount, logsz@(logwidth, logheight)) = (log2i count, mapT log2i sz)
>          logarea = logheight + logwidth + logcount :: Double
>          sizeGuess = ceiling (logarea / 2.0) :: Int
>          tsz = mapT (2^) logreal
>          tsz2d = uncurry GL.TextureSize2D . bothFromI $ tsz
>          logreal, counts :: (Int, Int)
>          (logreal, counts) = fromJust $ trial False (sizeGuess, sizeGuess)
>              where trial dir tr = if ((foldT (&&) $ zipT (\a b -> fromIntegral a < b) tr logsz) ||
>                                       fromIntegral (foldT (+) tr) < logarea) then Nothing else minimumBy comp (Just (tr, cnts): ts)
>                        where ts = if dir then [lft] else [lft, rt]
>                              (lft, rt) = (trial True $ first (subtract 1) tr, trial False $ second (subtract 1) tr)
>                              comp = comparing (maybe (fromIntegral sizeGuess*2+1) (\(a, b) -> pnorm 1 . bothFromI $ a))
>                              cnts = mapT (max 1 . floor . (2.0**)) $ bothFromI tr - logsz --Equivalent to trial size / sz

> tinyFont = fontData . breakEvery 5 . breakEvery 3 $ tinyFontData 0 -}

> data MonoFont = MonoFont (Int, Int) (M.Map (Int, Char) [GLubyte])

> fromByteList :: [[[[GLubyte]]]] -> MonoFont
> fromByteList xs = MonoFont charsz $ M.fromList (zip indices chunks)
>  where chunks = concat . concat $ xs
>        indices = concatMap (zipWith (,) [0..] . concat . replicate (snd charsz)) . breakEvery (fst linesz) . map (chr) $ [32..]
>        (linesz, charsz) = let (a@(b@(c:_):_):_) = xs in ((length b, length xs), (length c, length a))

> ceilPow = mapT ((2^) . ceiling . log2i)

> mapInit f xs = map f (init xs) ++ [last xs]

> replace x y = concatMap (\z -> if z == x then y else [z])

> data TextRenderContext = TextRenderContext {txtFont :: MonoFont, txtMaxWidth, tabSize :: Int, wrapChar :: Maybe Char}

> initTail [x]    =  ([], x)
> initTail (x:xs) =  (x : init ys, v) where (ys,v) = initTail xs
> initTail []     =  error "initTail: empty list"

> drawText :: MonoFont -> Int -> Int -> String -> IO TextureObject
> drawText (MonoFont charsz tex) maxWidth tabSize str = do
>      [texName] <- genObjectNames 1
>      textureBinding Texture2D $= Just texName
>      textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
>      GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA' tsz2d 0 $ GL.PixelData GL.RGBA GL.UnsignedByte dat
>      textureBinding Texture2D $= Nothing
>      return texName
>  where ls = lines . replace '\t' (replicate tabSize ' ') $ str
>        width = maximum . (if maxWidth == -1 then id else (maxWidth:)) $ map length ls
>        wrappedLines = concatMap (wrapLine . breakEvery (width-1)) ls
>            where wrapLine (Just c) = foldT (\x -> (x++).return) . first (map (++[c])) . initTail
>                  wrapLine Nothing = id
>        tsz = ceilPow $ charsz * (width, length wrappedLines)
>        tsz2d = uncurry GL.TextureSize2D . bothFromI $ tsz
>        dat = padRight (snd tsz (replicate (fst tsz) 0)) . concat $
>              concatMap (\line -> padRight (fst tsz) 0 $ concatMap (\ix -> map (lookup ix) line) [0..snd charsz-1]) wrappedLines
>            where lookup ix x = maybe (tex M.! (ix, ' ')) id $ tex `M.lookup` (ix, x)

fontData :: [[[[GLubyte]]]] -> IO Font
fontData xs = do
     [texName] <- genObjectNames 1
     textureBinding Texture2D $= Just texName
     textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
     generateMipmap Texture2D $= Enabled
     newArray texData >>= GL.texImage2D Nothing GL.NoProxy 0 GL.RGBA' tsz2d 0 . GL.PixelData GL.RGBA GL.UnsignedByte
     textureBinding Texture2D $= Nothing
     return $ Font tsz linesz charsz texName
 where 
       texData = concat . padRight (snd tsz) (replicate (fst tsz * 4) 0) . map ( concatMap (
                 padRight (fst tsz * 4) 0 . concatMap (concatMap (\x -> [x,x,x,255])))) $ xs :: [GLubyte]
       tsz = mapT ((2^) . ceiling . log2i) $ linesz * charsz
       tsz2d = uncurry GL.TextureSize2D . bothFromI $ tsz

> {- fontArr xs = newArray texData >>= \x -> return (tsz, x)
>  where (linesz, charsz) = let (a@(b@(c:_):_):_) = xs in ((length b, length xs), (length c, length a))
>        texData :: [GLubyte]
>        texData = concat . padRight (snd tsz) (replicate (fst tsz * 4) 0) . map ( concatMap (
>                  padRight (fst tsz * 4) 0 . concatMap (concatMap (\x -> [x,0,0,255])))) $ xs
>        tsz = mapT ((2^) . ceiling . log2i) $ linesz * charsz -}

tinyFont = fontData tinyFontData 

> corner dir = case dir of 0 -> (0,0); 1 -> (1,0); 2 -> (1,1); 3 -> (0,1)

> monouvs :: MonoFont -> (Int,Int) -> Int -> Char -> (GL.GLfloat, GL.GLfloat)
> monouvs (MonoFont charsz m) imgsz corn ch | chix >= 0 && chix < foldT (*) linesz = zipT (/) pos . mapT fromIntegral $ imgsz
>     where pos = bothFromI $ charsz * (corner corn + flipT (chix `divMod` lineWidth))
>           chix = ord ch - 32
>           linesz@(lineWidth, _) = zipT (floor . (/)) imgsz charsz