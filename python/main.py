from ctypes import Structure, sizeof
import sys
import atexit

from OpenGL.GL import *
import glfw

class Shader:
    def __init__(self):
        self.handle = glCreateProgram()
    
    def attach_shader(self, content, type, log_always=False):
        shader = glCreateShader(type)
        glShaderSource(shader, [content])
        glCompileShader(shader)

        status = ctypes.c_uint(GL_UNSIGNED_INT)
        glGetShaderiv(shader, GL_COMPILE_STATUS, status)
        if log_always or not status:
            print(glGetShaderInfoLog(shader).decode("utf-8"), file=sys.stderr)
            glDeleteShader(shader)
            return False
        
        glAttachShader(self.handle, shader)
        glDeleteShader(shader)
        return True

    def link(self, log_always=False):
        glLinkProgram(self.handle)
        status = ctypes.c_uint(GL_UNSIGNED_INT)
        glGetProgramiv(self.handle, GL_LINK_STATUS, status)
        if log_always or not status:
            print(glGetProgramInfoLog(self.handle).decode("utf-8"), file=sys.stderr)
            return False
        return True
    
    def use(self):
        glUseProgram(self.handle)

    def unuse(self):
        glUseProgram(0)

def main():
    if not glfw.init():
        raise RuntimeError("failed to initialize GLFW")
    atexit.register(glfw.terminate)

    window = glfw.create_window(700, 700, "triangle", None, None)
    if not window:
        raise RuntimeError("failed to create GLFWwindow")

    glfw.window_hint(glfw.CONTEXT_VERSION_MAJOR, 4)
    glfw.window_hint(glfw.CONTEXT_VERSION_MINOR, 6)
    glfw.make_context_current(window)



    glClearColor(0, 0, 0, 1)
    while glfw.window_should_close(window) == glfw.FALSE:
        glClear(GL_COLOR_BUFFER_BIT)


        glfw.swap_buffers(window)
        glfw.wait_events()

if __name__ == "__main__":
    main()