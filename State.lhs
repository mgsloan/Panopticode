> module State where
> import Edge
> import Data.List (partition)
> import Data.Graph.Inductive.Query.Monad ((><))
> import qualified Graphics.UI.GLUT as GL
> import Data.Char (isSpace)
> import Data.List(intersperse)
> import Data.Maybe(maybe)
> import Data.Geom2.Interval
> import Font
> import TinyFont
> import System.Directory
> import System.IO
> import System.FilePath

> data Buffer = Buffer { bufferName, bufferText :: String }

> data Chunk = Chunk { chunkBuffer :: Buffer, chunkIvl :: Interval Int, shapeCache :: Maybe Channel, textureCache :: Maybe GL.TextureObject }

> data MouseState = MouseState { buttonState :: [Bool], mousePos :: GL.Position } deriving Show

> data EditorState = EditorState {bufs ::[Buffer], mouseState :: MouseState, editorOffset :: (GL.GLfloat, GL.GLfloat), editorFnt :: MonoFont}

> data Channel = Channel { leftEdge, rightEdge :: Edge GL.GLuint } deriving Show

 fillShapeData :: [Buffer] -> [Buffer]
 fillShapeData = map (\x -> x { shapeCache = Just (bufferShape x) })

 bufferShape :: Buffer -> Channel
 bufferShape b = maybe (Channel l r) id $ shapeCache b
   where dat = map ((fromIntegral . length >< fromIntegral . length) . span (isSpace)) . lines . bufferText $ b
         l = map fst dat
         r = zipWith (+) l $ map snd dat

> fillTextureData :: TextRenderContext -> [Buffer] -> IO [Buffer]
> fillTextureData cont = mapM (\x -> bufferTexture cont x >>= \tex -> return (x { textureCache = Just tex }) )

> bufferTexture :: TextRenderContext -> Buffer -> IO GL.TextureObject
> bufferTexture cont b = do
>   tex <- drawText (txtFont cont) (processContext cont (bufferText b))
>   return . maybe tex id $ textureCache b

> readBuffer :: (Int, Int) -> FilePath -> IO Buffer
> readBuffer p x = readFile x >>= \str -> return $ Buffer (takeFileName x) str p Nothing Nothing

> initialBuffers = [Buffer "b" "Out of the night when the moon was bright" (0,0) Nothing Nothing]
> initialState = do 
>   bufs <- getCurrentDirectory >>= getDirectoryContents >>= mapM (readBuffer (0,0)) . filter ((==".lhs").takeExtension)
>   return (EditorState [head bufs] initialMouseState (0,0) tinyFontData)
> initialMouseState = MouseState [] (GL.Position 0 0)

> tick :: Int -> EditorState -> EditorState
> tick t = id