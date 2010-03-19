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

> idle state = do
>  env <- GL.get state
>  time <- GL.get GL.elapsedTime
>  newbufs <- fillTextureData (TextRenderContext (editorFnt env) (-1) 4 "") (bufs env)
>  state $= tick time (env { bufs = newbufs })
>  GL.postRedisplay Nothing

> keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
>   GL.destroyWindow window
>   exitWith ExitSuccess
> keyboardMouse _ state key st mod pos = do
>  env <- GL.get state
>  print (mouseState $ env)
>  state $= userAction env key st mod pos
> mouseMoveHandler state pos = do
>  env <- GL.get state
>  state $= mouseMove env pos

> setMouse :: EditorState -> GL.MouseButton -> GL.KeyState -> EditorState
> setMouse x mb st = x { mouseState = ( mst { buttonState = mutateList num False (st==GL.Down) $ buttonState mst }) }
>     where num = case mb of GL.LeftButton -> 0; GL.MiddleButton -> 1; GL.RightButton -> 2
>           mst = mouseState x

> userAction :: EditorState -> GL.Key -> GL.KeyState -> GL.Modifiers -> GL.Position -> EditorState
> userAction x (GL.MouseButton mb) st _ _ = setMouse x mb st
> userAction x _ _ _ _ = x

> mouseMove :: EditorState -> GL.Position -> EditorState
> mouseMove st p@(GL.Position x y) = st { mouseState = oldState { mousePos = p },
>      editorOffset = editorOffset st + (if (not . checkList False 0 . buttonState $ oldState) then (0,0) else offset) }
>     where oldState = mouseState st
>           oldPos@(GL.Position ox oy) = (mousePos oldState)
>           offset = (fromIntegral (ox-x), fromIntegral (y-oy))