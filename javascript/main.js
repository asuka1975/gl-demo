vs = `#version 300 es

in vec2 vertex;
in vec4 color;

out vec4 vertexColor;

void main() {
    gl_Position = vec4(vertex, 0, 1);
    vertexColor = color;
}
`;

fs = `#version 300 es
precision highp float;

in vec4 vertexColor;

out vec4 outColor;

void main() {
    outColor = vertexColor;
}
`;

function attachShader(gl, program, src, type) {
    const shader = gl.createShader(type);
    gl.shaderSource(shader, src);
    gl.compileShader(shader);
    if(!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
        alert(gl.getShaderInfoLog(shader));
    }
    gl.attachShader(program, shader);
    gl.deleteShader(shader);
}

function main() {
    const canvas = document.querySelector("#scene");
    const gl = canvas.getContext("webgl2");

    if(gl === null) {
        alert("failed initialize WebGL");
        return ;
    }

    const vertex = [
         0.0,  1.0,       1.0, 0.0, 0.0, 1.0,
        -1.0, -1.0,       0.0, 1.0, 0.0, 1.0,
         1.0, -1.0,       0.0, 0.0, 1.0, 1.0
    ];

    const program = gl.createProgram();
    attachShader(gl, program, vs, gl.VERTEX_SHADER);
    attachShader(gl, program, fs, gl.FRAGMENT_SHADER);
    gl.linkProgram(program);

    const vtx = gl.createBuffer();
    gl.bindBuffer(gl.ARRAY_BUFFER, vtx);
    gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertex), gl.STATIC_DRAW);

    const vao = gl.createVertexArray();
    gl.bindVertexArray(vao);
    const loc1 = gl.getAttribLocation(program, "vertex");
    const loc2 = gl.getAttribLocation(program, "color");
    gl.enableVertexAttribArray(loc1);
    gl.enableVertexAttribArray(loc2);
    gl.vertexAttribPointer(loc1, 2, gl.FLOAT, false, 24, 0);
    gl.vertexAttribPointer(loc2, 4, gl.FLOAT, false, 24, 8);
    gl.bindVertexArray(null);

    gl.clearColor(0, 0, 0, 1);
    gl.clear(gl.COLOR_BUFFER_BIT);

    gl.useProgram(program);
    gl.bindVertexArray(vao);
    gl.drawArrays(gl.TRIANGLES, 0, 3);
    gl.bindVertexArray(null);
    gl.useProgram(null);
}

window.onload = main;