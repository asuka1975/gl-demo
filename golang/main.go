package main

import (
	"github.com/go-gl/gl/v4.6-core/gl"
	"github.com/go-gl/glfw/v3.3/glfw"
)


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
	

	gl.ClearColor(0, 0, 0, 1)
	for !window.ShouldClose() {
		gl.Clear(gl.COLOR_BUFFER_BIT)

		window.SwapBuffers()
		glfw.PollEvents()
	}
}