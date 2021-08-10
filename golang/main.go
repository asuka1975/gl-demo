package main

import (
	"fmt"
	"strings"
	"io/ioutil"
	"runtime"

	"github.com/go-gl/gl/v4.6-core/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)

func attachShader(program uint32, src string, tp uint32) error {
	shader := gl.CreateShader(tp)

	csources, free := gl.Strs(src)
	gl.ShaderSource(shader, 1, csources, nil)
	free()
	gl.CompileShader(shader)
	var status int32
	gl.GetShaderiv(shader, gl.COMPILE_STATUS, &status)
	if status == gl.FALSE {
		var logLength int32
		gl.GetShaderiv(shader, gl.INFO_LOG_LENGTH, &logLength)

		log := strings.Repeat("\x00", int(logLength + 1))
		gl.GetShaderInfoLog(shader, logLength, nil, gl.Str(log))
		return fmt.Errorf("%v", log)
	}

	gl.AttachShader(program, shader)
	gl.DeleteShader(shader)
	return nil
}

func init() {
	runtime.LockOSThread()
}

func main() {
	if err := glfw.Init(); err != nil {
		panic(err)
	}
	defer glfw.Terminate()

	window, err := glfw.CreateWindow(700, 700, "triangle", nil, nil)
	if err != nil {
		panic(err)
	}
	glfw.WindowHint(glfw.ContextVersionMajor, 4)
	glfw.WindowHint(glfw.ContextVersionMinor, 6)
	window.MakeContextCurrent()

	if err := gl.Init(); err != nil {
		panic(err)
	}

	vertex := []float32 {
		0, 1, 1, 0, 0, 1,
		-1, -1, 0, 1, 0, 1,
		1, -1, 0, 0, 1, 1,
	}
	
	var vbo uint32
	gl.GenBuffers(1, &vbo)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.BufferData(gl.ARRAY_BUFFER, len(vertex) * 4, gl.Ptr(vertex), gl.STATIC_DRAW)
	defer gl.DeleteBuffers(1, &vbo)

	var vao uint32
	gl.GenVertexArrays(1, &vao)
	gl.BindVertexArray(vao)
	gl.BindBuffer(gl.ARRAY_BUFFER, vbo)
	gl.EnableVertexAttribArray(0)
	gl.EnableVertexAttribArray(1)
	gl.VertexAttribPointerWithOffset(0, 2, gl.FLOAT, false, 24, 0)
	gl.VertexAttribPointerWithOffset(1, 4, gl.FLOAT, false, 24, 8)
	gl.BindVertexArray(0)
	defer gl.DeleteVertexArrays(1, &vao)

	program := gl.CreateProgram()
	bytes, _ := ioutil.ReadFile("../shaders/triangle.vert")
	attachShader(program, string(bytes) + "\x00", gl.VERTEX_SHADER)
	bytes, _ = ioutil.ReadFile("../shaders/triangle.frag")
	attachShader(program, string(bytes) + "\x00", gl.FRAGMENT_SHADER)
	gl.LinkProgram(program)
	defer gl.DeleteProgram(program)

	gl.ClearColor(0, 0, 0, 1)
	for !window.ShouldClose() {
		gl.Clear(gl.COLOR_BUFFER_BIT)

		gl.BindVertexArray(vao)
		gl.UseProgram(program)
		gl.DrawArrays(gl.TRIANGLES, 0, 3)
		gl.UseProgram(0)
		gl.BindVertexArray(0)

		window.SwapBuffers()
		glfw.WaitEvents()
	}
}