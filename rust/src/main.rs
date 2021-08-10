extern crate gl;
extern crate glfw;

use gl::types::*;
use glfw::Context;
use std::{mem, ptr};
use std::fs::File;
use std::io::prelude::*;
use std::ffi::CString;

struct Shader {
    m_handle: GLuint, 
}
impl Shader {
    pub fn new() -> Self {
        unsafe {
            Self { m_handle: gl::CreateProgram() }
        }
    }
    pub fn handle(&self) -> GLuint {
        self.m_handle
    }
    pub fn attach_shader(&self, src: &String, tp: GLenum) {
        unsafe {
            let shader = gl::CreateShader(tp);
            let cs = CString::new(src.as_bytes()).unwrap();
            gl::ShaderSource(shader, 1, &mut cs.as_ptr(), ptr::null());
            gl::CompileShader(shader);

            let mut status = gl::FALSE as GLint;
            gl::GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);
            if status != (gl::TRUE as GLint) {
                let mut len = 0;
                gl::GetShaderiv(shader, gl::INFO_LOG_LENGTH, &mut len);
                let mut buf = vec![0u8; len as usize - 1];
                gl::GetShaderInfoLog(shader, len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
                panic!("{}", String::from_utf8(buf).expect("ShaderInfoLog not valid utf8"));
            }
            gl::AttachShader(self.handle(), shader);
            gl::DeleteShader(shader)
        }
    }
    pub fn link(&self) {
        unsafe {
            gl::LinkProgram(self.handle());
            let mut status = gl::FALSE as GLint;
            gl::GetProgramiv(self.handle(), gl::LINK_STATUS, &mut status);
            if status != (gl::TRUE as GLint) {
                let mut len = 0;
                gl::GetProgramiv(self.handle(), gl::INFO_LOG_LENGTH, &mut len);
                let mut buf = vec![0u8; len as usize - 1];
                gl::GetProgramInfoLog(self.handle(), len, ptr::null_mut(), buf.as_mut_ptr() as *mut GLchar);
                panic!("{}", String::from_utf8(buf).expect("ShaderInfoLog not valid utf8"));
            }
        }
    }
    pub fn use_(&self) {
        unsafe {
            gl::UseProgram(self.handle());
        }
    }
    pub fn unuse(&self) {
        unsafe {
            gl::UseProgram(0);
        }
    }
}
impl Drop for Shader {
    fn drop(&mut self) {
        if self.m_handle != 0 {
            unsafe {
                gl::DeleteProgram(self.m_handle);
                self.m_handle = 0;
            }
        }
    }
}

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    let (mut window, _) = glfw.create_window(700, 700, "triangle", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFWwindow");
    glfw.window_hint(glfw::WindowHint::ContextVersion(4, 6));
    window.make_current();

    gl::load_with(|symbol| window.get_proc_address(symbol) as *const _);

    let vertex: [GLfloat; 18] = [
        0.0, 1.0,     1.0, 0.0, 0.0, 1.0,
        -1.0, -1.0,   0.0, 1.0, 0.0, 1.0,
        1.0, -1.0,    0.0, 0.0, 1.0, 1.0
    ];

    let mut vtx: GLuint = 0;
    let mut vao: GLuint = 0;

    unsafe {
        gl::GenBuffers(1, &mut vtx);
        gl::BindBuffer(gl::ARRAY_BUFFER, vtx);
        gl::BufferData(gl::ARRAY_BUFFER, (vertex.len() * mem::size_of::<GLfloat>()) as GLsizeiptr, mem::transmute(&vertex[0]), gl::STATIC_DRAW);

        gl::GenVertexArrays(1, &mut vao);
        gl::BindVertexArray(vao);
        gl::BindBuffer(gl::ARRAY_BUFFER, vtx);
        gl::EnableVertexAttribArray(0);
        gl::EnableVertexAttribArray(1);
        gl::VertexAttribPointer(0, 2, gl::FLOAT, gl::FALSE as GLboolean, 24, ptr::null());
        gl::VertexAttribPointer(1, 4, gl::FLOAT, gl::FALSE as GLboolean, 24, 8 as *const GLvoid);
    }

    let program = Shader::new();
    let mut vs = String::new();
    let mut fp1 = File::open("../shaders/triangle.vert").expect("file not found");
    fp1.read_to_string(&mut vs)
        .expect("failed to read vertex shader");
    program.attach_shader(&vs, gl::VERTEX_SHADER);
    let mut fs = String::new();
    let mut fp2 = File::open("../shaders/triangle.frag").expect("file not found");
    fp2.read_to_string(&mut fs)
        .expect("failed to read fragment shader");
    program.attach_shader(&fs, gl::FRAGMENT_SHADER);
    program.link();

    unsafe {
        gl::ClearColor(0.0, 0.0, 0.0, 1.0);
    }
    while !window.should_close() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT);

            program.use_();
            gl::BindVertexArray(vao);
            gl::DrawArrays(gl::TRIANGLES, 0, 3);
            gl::BindVertexArray(0);
            program.unuse();
        }

        window.swap_buffers();
        glfw.wait_events();
    }
    unsafe {
        gl::DeleteVertexArrays(1, &mut vao);
        gl::DeleteBuffers(1, &mut vtx);
    }
}
