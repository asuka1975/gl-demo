module Main where

import Control.Monad        (unless, void, when)
import Control.Monad.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.State  (liftIO)

import qualified Graphics.Rendering.OpenGL as GL
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
                vbo <- genVbo
                vao <- genVao
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