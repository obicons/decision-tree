(require 'asdf)
(require 'sb-posix)

(setf asdf:*central-registry*
      '(*default-pathname-defaults*
        (car (directory (sb-posix:getcwd)))))

(asdf:operate 'asdf:load-op "decision-tree")
