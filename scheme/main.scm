(use gauche.uvector)
(use gl)
(use gl.glfw)

(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
      (begin
        (close-input-port p)
        (list->string (reverse ls1)))
      (loop (cons c ls1) (read-char p))))))

(define (attach-shader program src type)
  (let1 shader (gl-create-shader type)
    (gl-shader-source shader (list src))
    (gl-compile-shader shader)
    (gl-attach-shader program shader)
    (gl-delete-shader shader)))

(define (main args)
  (unless (glfw-init) 
    (display "failed to initialize GLFW" (current-error-port))
    (exit 1))
  (let ((window  (glfw-create-window 700 700 "triangle" #f #f)))
    (unless window
      (display "failed to create GLFWwindow" (current-error-port))
      (exit 1))
    (glfw-window-hint GLFW_CONTEXT_VERSION_MAJOR 4)
    (glfw-window-hint GLFW_CONTEXT_VERSION_MAJOR 6)
    (glfw-make-context-current window)

    (let ((vtx     (gl-gen-buffers 1))
          (vao     (gl-gen-vertex-arrays 1))
          (program (gl-create-program))
          (vertex  '#f32( 0  1    1 0 0 1
                         -1 -1    0 1 0 1
                          1 -1    0 0 1 1)))
      (gl-bind-vertex-array (uvector-ref vao 0))
      (gl-bind-buffer GL_ARRAY_BUFFER (uvector-ref vtx 0))
      (gl-buffer-data GL_ARRAY_BUFFER (* 18 4) vertex GL_STATIC_DRAW)
      (gl-enable-vertex-attrib-array 0)
      (gl-enable-vertex-attrib-array 1)
      (gl-vertex-attrib-pointer 0 2 GL_FLOAT #f 24 0)
      (gl-vertex-attrib-pointer 1 4 GL_FLOAT #f 24 8)
      (gl-bind-vertex-array 0)

      (attach-shader program (read-file "../shaders/triangle.vert") GL_VERTEX_SHADER)
      (attach-shader program (read-file "../shaders/triangle.frag") GL_FRAGMENT_SHADER)
      (gl-link-program program)

      (gl-point-size 10)
      (gl-clear-color 0 0 0 1)
      (let loop () 
        (unless (glfw-window-should-close window)
          (gl-clear GL_COLOR_BUFFER_BIT)

          (gl-use-program program)
          (gl-bind-vertex-array (uvector-ref vao 0))
          (gl-draw-arrays GL_TRIANGLES 0 3)
          (gl-bind-vertex-array 0)
          (gl-use-program 0)

          (glfw-swap-buffers window)
          (glfw-wait-events)
          (loop)))
      (gl-delete-program program)
      (gl-delete-buffers vtx)
      (gl-delete-vertex-arrays vao)
      (glfw-destroy-window window))

    (glfw-terminate)
    (exit 0)))