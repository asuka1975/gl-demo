extern crate gl;
extern crate glfw;

use glfw::{Context};

fn main() {
    let mut glfw = glfw::init(glfw::FAIL_ON_ERRORS).unwrap();

    let (mut window, _) = glfw.create_window(700, 700, "triangle", glfw::WindowMode::Windowed)
        .expect("Failed to create GLFWwindow");
    glfw.window_hint(glfw::WindowHint::ContextVersion(4, 6));
    window.make_current();

    while !window.should_close() {
        window.swap_buffers();
        glfw.wait_events();
    }
}
