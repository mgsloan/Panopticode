> {-# OPTIONS -fglasgow-exts #-}
> module Render(display, initGL) where

> import Data.IORef
> import Graphics.UI.GLUT hiding (Font)
> import Graphics.UI.GLUT (($=))
> import System.Exit
> import State
> import Utils
> import Font
> import Control.Arrow ((***))
> import Debug.Trace
> import TinyFont

> display state = do
>  clear [ColorBuffer , DepthBuffer]
>  env <- get state
>  render env
>  swapBuffers

> vert ::(GLfloat,GLfloat,GLfloat) -> (GLfloat, GLfloat) -> IO ()
> vert (x, y, z) (u, v) =
>   do texCoord (TexCoord2 u v)
>      vertex (Vertex3 x y z)

 render :: EditorState -> IO ()
 render st = do
   --(sz, arr) <- fontArr tinyFontData
   --rasterPos (Vertex3 (-2) 0 (-10) :: Vertex3 GLfloat)
   --drawPixels (uncurry Size . bothFromI $ sz) $ PixelData RGBA UnsignedByte arr
   texture Texture2D $= Enabled
   textureBinding Texture2D $= (Just $ fntTex fnt)
   translate (Vector3 (-posx) posy (0.0 :: GLfloat))
   mapM_ (mapM_ renderLine . zip [0,charh+1..] . lines . bufferText) . bufs $ st
   translate (Vector3 posx (-posy) (0.0 :: GLfloat))
   flush
   reportErrors
   texture Texture2D $= Disabled
     where renderLine (row, line) = renderPrimitive Quads (mapM_ (drawChar row) . zip [0.0,charw+1..] $ line)
           drawChar row (col,c) = do vert (0,0,0) (col  ,row,  -95) (monouvs fnt 0 c)
                                     vert (0,0,0) (col+3,row,  -95) (monouvs fnt 1 c)
                                     vert (0,0,0) (col+3,row+5,-95) (monouvs fnt 2 c)
                                     vert (0,0,0) (col  ,row+5,-95) (monouvs fnt 3 c)
           fnt = editorFnt st; (posx, posy) = editorOffset st; (charw, charh) = bothFromI $ fntCharSize fnt

> render st = do
>    let b1 = head $ bufs st;
>        (Just tex) = textureCache b1
>    texture Texture2D $= Enabled
>    textureBinding Texture2D $= (Just $ tex)
>    translate (Vector3 (-posx) posy (0.0 :: GLfloat))
>    (TextureSize2D width height) <- get (textureSize2D (Left Texture2D) 0)
>    renderPrimitive Quads (do vert (0,0,-95) (0,0)
>                              vert (fromIntegral width,0,-95) (1,0)
>                              vert (fromIntegral width,fromIntegral height,-95) (1,1)
>                              vert (0,fromIntegral height,-95) (0,1))
>    translate (Vector3 posx (-posy) (0.0 :: GLfloat))
>    texture Texture2D $= Disabled
>  where (posx, posy) = editorOffset st

>
> initGL name = do
>  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer, WithStencilBuffer, Multisampling]
>  initialWindowSize $= Size 640 480
>  initialWindowPosition $= Position 0 0
>  window <- createWindow name
>  clearColor $= Color4 0 0 0 0
>  viewport $= (Position 0 480, Size 640 480)
>  matrixMode $= Projection
>  loadIdentity
>  ortho 0.0 640 480 0.0 0.0 100.0
>  matrixMode $= Modelview 0
>  --blend $= Enabled
>  --blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
>  --multisample $= Enabled
>  textureFunction $= Decal
>  return window