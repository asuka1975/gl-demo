(require 'cffi)
(require 'cl-opengl)
(require 'cl-glfw3)

(defvar *vtx*)
(defvar *vao*)
(defvar *program*)

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defun attach-shader (program src type)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader src)
    (gl:compile-shader shader)
    (gl:attach-shader program shader)))

(defun render ()
  (gl:clear :color-buffer)
  (gl:use-program *program*)
  (gl:bind-vertex-array *vao*)
  (gl:draw-arrays :triangles 0 3)
  (gl:bind-vertex-array 0)
  (gl:use-program 0))

(defun main ()
  (glfw:with-init-window (:width 700 :height 700 :title "triangle"
                            :context-version-major 4
                            :context-version-minor 6)
    (let ((buffers (gl:gen-buffers 1))
          (vertex  (gl:alloc-gl-array :float 18))
          (data    #( 0.0  1.0    1.0 0.0 0.0 1.0
                     -1.0 -1.0    0.0 1.0 0.0 1.0
                      1.0 -1.0    0.0 0.0 1.0 1.0)))
      (setf *vtx* (elt buffers 0))
      (dotimes (i (length data))
        (setf (gl:glaref vertex i) (aref data i)))
      (gl:bind-buffer :array-buffer *vtx*)
      (gl:buffer-data :array-buffer :static-draw vertex)
      (gl:free-gl-array vertex))

    (setf *vao* (gl:gen-vertex-array))
    (gl:bind-vertex-array *vao*)
    (gl:bind-buffer :array-buffer *vtx*)
    (gl:enable-vertex-attrib-array 0)
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 0 2 :float nil 24 (cffi:null-pointer))
    (gl:vertex-attrib-pointer 1 4 :float nil 24 (cffi:make-pointer 8))
    (gl:bind-vertex-array 0)

    (setf *program* (gl:create-program))
    (attach-shader *program* (read-file "../shaders/triangle.vert") :vertex-shader)
    (attach-shader *program* (read-file "../shaders/triangle.frag") :fragment-shader)
    (gl:link-program *program*)

    (gl:clear-color 0 0 0 1)
    (loop until (glfw:window-should-close-p)
      do (render)
      do (glfw:swap-buffers)
      do (glfw:wait-events))
    (gl:delete-program *program*)
    (gl:delete-buffers (list *vtx*))
    (gl:delete-vertex-arrays (list *vao*))))