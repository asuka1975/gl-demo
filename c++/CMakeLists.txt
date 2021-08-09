cmake_minimum_required(VERSION 3.14)

project(gl)

add_executable(triangle main.cpp)

target_link_libraries(triangle glfw GL)