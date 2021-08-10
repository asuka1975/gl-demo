extern crate gl;
extern crate glfw;

use gl::types::*;
use glfw::Context;

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    let (mut window, _) = glfw.create_window(700, 700, "triangle", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFWwindow");
    glfw.window_hint(glfw::WindowHint::ContextVersion(4, 6));
    window.make_current();

    gl::load_with(|symbol| window.get_proc_address(symbol) as *const _);

    unsafe {
        gl::ClearColor(0.0, 0.0, 0.0, 1.0);
    }
    while !window.should_close() {
        unsafe {
            gl::Clear(gl::COLOR_BUFFER_BIT);
        }
        

        window.swap_buffers();
        glfw.wait_events();
    }
}
