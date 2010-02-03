> module State where
> import Edge
> import Data.List (partition)
> import Data.Graph.Inductive.Query.Monad ((><))
> import qualified Graphics.UI.GLUT as GL
> import Data.Char (isSpace)
> import Data.List(intersperse)
> import Data.Maybe(maybe)
> import Font
> import System.Directory
> import System.IO
> import System.FilePath

> data Buffer = Buffer { bufferName, bufferText :: String, bufferPos :: (Int, Int), shapeCache :: Maybe Channel }

> data MouseState = MouseState { buttonState :: [Bool], mousePos :: GL.Position } deriving Show

> data EditorState = EditorState {bufs ::[Buffer], mouseState :: MouseState, editorOffset :: (GL.GLfloat, GL.GLfloat), editorFnt :: Font}

> data Channel = Channel { leftEdge, rightEdge :: Edge GL.GLuint } deriving Show

> fillShapeData :: [Buffer] -> [Buffer]
> fillShapeData = map (\x -> x { shapeCache = Just (bufferShape x) })

> bufferShape :: Buffer -> Channel
> bufferShape b = maybe (Channel l r) id $ shapeCache b
>   where dat = map ((fromIntegral . length >< fromIntegral . length) . span (isSpace)) . lines . bufferText $ b
>         l = map fst dat
>         r = zipWith (+) l $ map snd dat

> readBuffer :: (Int, Int) -> FilePath -> IO Buffer
> readBuffer p x = readFile x >>= \str -> return $ Buffer (takeFileName x) str p Nothing

> initialBuffers = [Buffer "b" "Out of the night when the moon was bright" (0,0) Nothing]
> initialState = do 
>   bufs <- getCurrentDirectory >>= getDirectoryContents >>= mapM (readBuffer (0,0)) . filter ((==".lhs").takeExtension)
>   fnt <- tinyFont
>   return (EditorState [head bufs] initialMouseState (0,0) fnt)
> initialMouseState = MouseState [] (GL.Position 0 0)

> tick :: Int -> EditorState -> EditorState
> tick t = id