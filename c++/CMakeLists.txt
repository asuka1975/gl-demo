cmake_minimum_required(VERSION 3.14)

project(gl)

set(CMAKE_CXX_STANDARD 17)

add_executable(triangle main.cpp)

target_link_libraries(triangle glfw GL GLEW)