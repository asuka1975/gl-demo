#version 460

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec4 color;

layout (location = 0) out vec4 outColor;

void main() {
    gl_Position = vec4(vertex, 0, 1);
    outColor = color;
}