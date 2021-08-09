module Shader (
    attach
) where

import Data.ByteString
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

attach :: GL.Program -> GL.ShaderType -> ByteString -> IO ()
attach program tp src = do
    shader <- GL.createShader tp
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    GL.attachShader program shader
    GL.deleteObjectName shader