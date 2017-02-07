(defpackage :decision-tree-asd
  (:use :cl :asdf))

(in-package :decision-tree-asd)

(defsystem decision-tree
  :name "decision-tree"
  :version "0.7.0"
  :maintainer "Max"
  :license "MIT"
  :description "Decision tree implementation"
  :long-description "An implementation of decision trees."
  :components ((:file "tests"
                      :depends-on ("decision-tree" "utils"))
               (:file "decision-tree"
                      :depends-on ("utils"))
               (:file "utils")))
