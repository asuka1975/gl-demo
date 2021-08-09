#include <fstream>
#include <iostream>
#include <vector>

#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include <glm/glm.hpp>

class shader_program {
public:
    shader_program() {
        m_handle = glCreateProgram();
    }
    shader_program(const shader_program& obj) = delete;
    shader_program(shader_program&& obj) noexcept {
        if(this != &obj) {
            reset();
            m_handle = obj.m_handle;
            obj.m_handle = 0;
        }
    }
    ~shader_program() {
        reset();
    }
    [[nodiscard]] bool enabled() const noexcept {
        return m_handle != 0;
    }
    [[nodiscard]] GLuint handle() const noexcept {
        return m_handle;
    }
    void reset() {
        if(enabled()) {
            glDeleteProgram(handle());
            m_handle = 0;
        }
    }
    bool add_shader(const std::string &src, GLenum type) {
        if(!enabled()) return false;
        GLuint shader = glCreateShader(type);
        auto source = src.data();
        GLint length = src.size();
        glShaderSource(shader, 1, &source, &length);
        glCompileShader(shader);

        GLint status;
        glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
        if(status == GL_FALSE) {
            GLsizei size; glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &size);
            std::string log(size, 0);
            glGetShaderInfoLog(shader, log.length(), &size, log.data());
            std::cerr << log << std::endl;
            return false;
        }

        glAttachShader(handle(), shader);
        glDeleteShader(shader);
        return true;
    }
    bool link() {
        if(!enabled()) return false;
        glLinkProgram(handle());
        GLint status;
        glGetProgramiv(handle(), GL_LINK_STATUS, &status);
        if(status == GL_FALSE) {
            GLsizei size; glGetProgramiv(handle(), GL_INFO_LOG_LENGTH, &size);
            std::string log(size, 0);
            glGetProgramInfoLog(handle(), log.length(), &size, log.data());
            std::cerr << log << std::endl;
            return false;
        }
        return true;
    }
    void use() const {
        glUseProgram(handle());
    }
    void unuse() const {
        glUseProgram(0);
    }
private:
    GLuint m_handle;
};

struct vertex_t {
    glm::vec2 position;
    glm::vec4 color;
};

int main() {
    if(glfwInit() == GLFW_FALSE) {
        throw std::runtime_error("failed to initialize GLFW");
    }
    std::atexit(glfwTerminate);

    auto window = glfwCreateWindow(700, 700, "triangle", nullptr, nullptr);
    if(window == nullptr) {
        throw std::runtime_error("failed to create GLFWwindow");
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 6);
    glfwMakeContextCurrent(window);

    if(glewInit() != GLEW_OK) {
        throw std::runtime_error("failed to initialize GLEW");
    }

    std::vector<vertex_t> vertex {
        { { 0, 1 }, { 1, 0, 0, 1 } },
        { { -1, -1 }, { 0, 1, 0, 1 } },
        { { 1, -1 }, { 0, 0, 1, 1 } }
    };

    GLuint vtx;
    glGenBuffers(1, &vtx);
    glBindBuffer(GL_ARRAY_BUFFER, vtx);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_t) * vertex.size(), vertex.data(), GL_STATIC_DRAW);

    GLuint vao;
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vtx);
    glEnableVertexAttribArray(0);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, sizeof(vertex_t), (GLvoid*)(offsetof(vertex_t, position)));
    glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(vertex_t), (GLvoid*)(offsetof(vertex_t, color)));
    glBindVertexArray(0);

    shader_program program;
    if(std::fstream fin("../shaders/triangle.vert", std::ios::in); fin.good()) {
        program.add_shader(std::string { std::istreambuf_iterator<char>{fin}, std::istreambuf_iterator<char>{} }, GL_VERTEX_SHADER);
    }
    if(std::fstream fin("../shaders/triangle.frag", std::ios::in); fin.good()) {
        program.add_shader(std::string { std::istreambuf_iterator<char>{fin}, std::istreambuf_iterator<char>{} }, GL_FRAGMENT_SHADER);
    }
    program.link();

    glClearColor(0, 0, 0, 1);
    while(glfwWindowShouldClose(window) == GLFW_FALSE) {
        glClear(GL_COLOR_BUFFER_BIT);

        program.use();
        glBindVertexArray(vao);
        glDrawArrays(GL_TRIANGLES, 0, 3);
        glBindVertexArray(0);
        program.unuse();

        glfwSwapBuffers(window);
        glfwWaitEvents();
    }
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vtx);

    return 0;
}