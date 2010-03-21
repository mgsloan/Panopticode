> {-# OPTIONS -fglasgow-exts #-}
> module Main(main) where

> import Data.IORef
> import Graphics.UI.GLUT (($=))
> import qualified Graphics.UI.GLUT as GL
> import System.Exit
> import Render
> import State
> import Utils
> import Font

> import Data.Geom2.D2

> type WindowId = Int

Here is the entry function of the program.  It passes off control to GLUT, after establishing a few callbacks.
I wish we didn't have to pass control over to GLUT - this should be amended at some point, I want to make sure
that it's impossible for the program to become unresponsive.  This means that all of the processing must take
place in threads which

A)  Execute rapidly
B)  Update state incrementally, such that visualizations of partially optimized layouts look decent

> main :: IO ()
> main = do
>  (progname, _) <- GL.getArgsAndInitialize
>  window <- initGL "Project Visualizer"
>  state <- initialState >>= newIORef
>  GL.displayCallback $= (display state)
>  GL.idleCallback $= Just (idle state)
>  GL.keyboardMouseCallback $= Just (keyboardMouse window state)
>  GL.motionCallback $= Just (mouseMoveHandler state)
>  GL.passiveMotionCallback $= Just (mouseMoveHandler state)
>  GL.mainLoop

> idle :: IORef EditorState -> IO ()
> idle state = do
>  env <- GL.get state
>  time <- GL.get GL.elapsedTime
>  state $= tick time env
>  GL.postRedisplay Nothing

> keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
>   GL.destroyWindow window
>   exitWith ExitSuccess
> keyboardMouse _ state key st mod pos = do
>  env <- GL.get state
>  state $= userAction env key st mod pos
> mouseMoveHandler state (GL.Position x y) = do
>  env <- GL.get state
>  state $= mouseMove env (fromIntegral x, fromIntegral y)

> setMouse :: EditorState -> GL.MouseButton -> GL.KeyState -> EditorState
> setMouse x mb st = x { editorMouse = ( mst { mouseButtons = mutateList num False (st==GL.Down) $ mouseButtons mst }) }
>     where num = case mb of GL.LeftButton -> 0; GL.MiddleButton -> 1; GL.RightButton -> 2
>           mst = editorMouse x

> userAction :: EditorState -> GL.Key -> GL.KeyState -> GL.Modifiers -> GL.Position -> EditorState
> userAction x (GL.MouseButton mb) st _ _ = setMouse x mb st
> userAction x _ _ _ _ = x

> mouseMove :: EditorState -> (Int, Int) -> EditorState
> mouseMove st p = st { editorMouse = old { mousePosition = p }, editorOffset = off }
>     where old = editorMouse st
>           off = editorOffset st + (if (not . checkList False 0 . mouseButtons $ old) then (0,0) else mousePosition old - p)