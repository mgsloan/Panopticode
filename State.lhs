> module State where
> import qualified Data.ByteString as B
> import qualified Data.ByteString.UTF8 as B
> import Data.List (partition)
> import Data.Char (isSpace)
> import Data.List (intersperse, findIndex)
> import Data.Maybe(maybe)
> import Data.Geom2.Interval
> import Data.Geom2.D2 (mapT)

> import qualified Graphics.UI.GLUT as GL
> import System.Directory
> import System.IO
> import System.FilePath
> import Control.Monad (mapM_)

> import Edge
> import Font
> import TinyFont

> data Buffer = Buffer {
>      bufferName   :: String,
>      bufferText   :: B.ByteString }

> class Planable a where
>     getShape    :: a -> Channel Int
>     drawTexture :: a -> IO GL.TextureObject

> data (Planable a) => Drawn a = Drawn {
>      objContent  :: a,
>      objShape    :: Channel Int,
>      objPosition :: (Int, Int),
>      objTexture  :: GL.TextureObject }

> freeResources :: (Planable a) => Drawn a -> IO ()
> freeResources o = GL.deleteObjectNames [objTexture o]

> newDrawn :: (Planable a) => (Int, Int) -> a -> IO (Drawn a)
> newDrawn p x = drawTexture x >>= return . Drawn x (getShape x) p

> changeContent :: (Planable a) => a -> Drawn a -> IO (Drawn a)
> changeContent x o = freeResources o >> drawTexture x >>= return . Drawn x (getShape x) (objPosition o)

> data Chunk = Chunk {
>      chunkSource   :: String,
>      chunkLines    :: [String],
>      chunkLineIvl  :: Interval Int,
>      chunkFont     :: MonoFont }

> instance Planable Chunk where
>      getShape ch = textShape (chunkFont ch) (chunkLines ch)
>      drawTexture ch = drawText (chunkFont ch) (chunkLines ch)

> data MouseState = MouseState {
>      mouseButtons :: [Bool],
>      mousePosition :: (Int, Int) } deriving Show

> data EditorState = EditorState {
>      editorBuffers :: [Buffer],
>      editorChunks  :: [Drawn Chunk],
>      editorMouse   :: MouseState,
>      editorOffset  :: (Int, Int),
>      editorFont    :: MonoFont }

> createChunk :: TextLayoutContext -> MonoFont -> Buffer -> Interval Int -> Chunk
> createChunk context font buf livl  = Chunk (bufferName buf) ls livl font
>     where ls = layoutText context . unlines . map B.toString . sliceList livl . B.lines . bufferText $ buf

> textShape :: MonoFont -> [String] -> Channel Int
> textShape fnt xs = Channel (concatMap (repl . maybe 0 id . findIndex isSpace) xs) (concatMap (repl . length) xs)
>     where repl = replicate (snd $ fntCharsz fnt)

> readBuffer :: String -> IO Buffer
> readBuffer file = B.readFile file >>= return . Buffer file

> initialState = do 
>   bufs <- getCurrentDirectory >>= getDirectoryContents >>= mapM readBuffer . filter ((==".lhs") . takeExtension)
>   let layout = TextLayoutContext (-1) 4 ""
>   chunks <- mapM (newDrawn (10,10)) [createChunk layout tinyFontData (bufs !! 0) (Interval 1 10)]
>   return (EditorState bufs chunks (MouseState [] (0,0)) (0,0) tinyFontData)

> tick :: Int -> EditorState -> EditorState
> tick t = id