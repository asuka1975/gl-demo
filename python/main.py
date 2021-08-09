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

class Vertex(Structure):
    _fields_ = [
        ('position', GLfloat * 2),
        ('color', GLfloat * 4)
    ]

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

    vertex = (Vertex * 3)(
        Vertex((0, 1), (1, 0, 0, 1)),
        Vertex((-1, -1), (0, 1, 0, 1)),
        Vertex((1, -1), (0, 0, 1, 1))
    )

    vtx = glGenBuffers(1)
    glBindBuffer(GL_ARRAY_BUFFER, vtx)
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex), vertex, GL_STATIC_DRAW)

    vao = glGenVertexArrays(1)
    glBindVertexArray(vao)
    glBindBuffer(GL_ARRAY_BUFFER, vtx)
    glEnableVertexAttribArray(0)
    glEnableVertexAttribArray(1)
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), GLvoidp(Vertex.position.offset))
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(Vertex), GLvoidp(Vertex.color.offset))
    glBindVertexArray(0)

    program = Shader()
    with open("../shaders/triangle.vert", "r") as f:
        program.attach_shader(f.read(), GL_VERTEX_SHADER)
    with open("../shaders/triangle.frag", "r") as f:
        program.attach_shader(f.read(), GL_FRAGMENT_SHADER)
    program.link()

    glClearColor(0, 0, 0, 1)
    while glfw.window_should_close(window) == glfw.FALSE:
        glClear(GL_COLOR_BUFFER_BIT)

        program.use()
        glBindVertexArray(vao)
        glDrawArrays(GL_TRIANGLES, 0, 3)
        glBindVertexArray(0)
        program.unuse()

        glfw.swap_buffers(window)
        glfw.wait_events()

    glDeleteProgram(program.handle)
    glDeleteVertexArrays(1, [vao])
    glDeleteBuffers(1, [vtx])

if __name__ == "__main__":
    main()