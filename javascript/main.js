vs = `#version 300 es

layout (location = 0) in vec2 vertex;
layout (location = 1) in vec4 color;

layout (location = 0) out vec4 outColor;

void main() {
    gl_Position = vec4(vertex, 0, 1);
    outColor = color;
}
`;

fs = `#version 300 es

layout (location = 0) in vec4 color;

layout (location = 0) out vec4 outColor;

void main() {
    outColor = color;
}
`;

function main() {
    const canvas = document.querySelector("#scene");
    const gl = canvas.getContext("webgl");

    if(gl === null) {
        alert("failed initialize WebGL");
        return ;
    }

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    
}

window.onload = main;