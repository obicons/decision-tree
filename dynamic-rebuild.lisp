(defpackage :dynamic-rebuild
  (:use :cl :sb-ext))

(in-package :dynamic-rebuild)

(defun pause-threads ()
  (progn (sb-thread:)))

(defun dynamic-rebuild (&rest pass-to-save-lisp)
  (when (zerop (sb-posix:fork))
    (apply #'save-lisp-and-die pass-to-save-lisp)))

(defun main ()
  (format t "Hello, world!~%"))

(format t "~a~%" (sb-thread:list-all-threads))

(save-lisp-and-die "test" :toplevel #'main)
(dynamic-rebuild "test" :toplevel #'main)
