module Main where

import Control.Monad         (unless, void, when)
import Control.Monad.Reader  (ReaderT, runReaderT, ask, asks)
import Control.Monad.State   (liftIO)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr           (plusPtr, nullPtr, Ptr)

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.UI.GLFW          as GLFW

import Lib

data GLHandle = GLHandle
    { window  :: !GLFW.Window
    , vtx     :: !GL.BufferObject
    , vao     :: !GL.VertexArrayObject
    , program :: !GL.Program
    }

type Env = ReaderT GLHandle IO

genVbo :: IO GL.BufferObject
genVbo = GL.genObjectName >>= return

genVao :: IO GL.VertexArrayObject
genVao = GL.genObjectName >>= return

offset :: Integral a => a -> Ptr b
offset = plusPtr nullPtr . fromIntegral

loopEnv :: Env a -> GLHandle -> IO a
loopEnv f handle = do
    runReaderT f handle

loop :: Env ()
loop = do
    window <- asks window

    liftIO $ do
        GLFW.swapBuffers window
        GLFW.waitEvents

    r <- liftIO $ GLFW.windowShouldClose window
    unless r loop

main :: IO ()
main = do
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow 700 700 "triangle" Nothing Nothing
        case m of 
            (Just window) -> do
                GLFW.makeContextCurrent m 
                --         | position |    |   color   |
                let vertex = [  0,  1,      1, 0, 0, 1
                             , -1, -1,      0, 1, 0, 1
                             ,  1, -1,      0, 0, 1, 1] :: [GL.GLfloat]

                vbo <- genVbo
                GL.bindBuffer GL.ArrayBuffer $= Just vbo
                withArray vertex $ \ptr -> do
                    let bufferSize = fromIntegral $ 4 * (length vertex)
                    GL.bufferData GL.ArrayBuffer $= (bufferSize, ptr, GL.StaticDraw)

                vao <- genVao
                GL.bindVertexArrayObject $= Just vao
                let verLoc = GL.AttribLocation 0
                    colLoc = GL.AttribLocation 1
                GL.vertexAttribArray   verLoc $= GL.Enabled
                GL.vertexAttribArray   colLoc $= GL.Enabled
                GL.vertexAttribPointer verLoc $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (4 * 6) (offset 0))
                GL.vertexAttribPointer colLoc $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float (4 * 6) (offset 8))
                GL.bindVertexArrayObject $= Nothing

                program <- GL.createProgram
                let handle = GLHandle { window  = window
                                      , vtx     = vbo
                                      , vao     = vao
                                      , program = program }

                loopEnv loop handle

                GL.deleteObjectName program
                GL.deleteObjectName vao
                GL.deleteObjectName vbo
            Nothing -> return ()
    GLFW.terminate