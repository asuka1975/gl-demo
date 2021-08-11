(require 'cl-opengl)
(require 'cl-glfw3)

(defun main ()
  (glfw:with-init-window (:width 700 :height 700 :title "triangle")
    (gl:clear-color 0 0 0 1)
    (loop until (glfw:window-should-close-p)
      do (glfw:swap-buffers)
      do (glfw:wait-events))))