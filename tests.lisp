(defpackage :tests
  (:use :cl :decision-tree :sb-ext)
  (:export :main))

(in-package :tests)

;;; Build process
;;(load "~/quicklisp/dists/quicklisp/software/slime-v2.18/swank-loader.lisp")
;; (swank-loader:init)
;; (asdf:load-system "decision-tree")


;;; decision tree for and 
(format t "~a~%" (macroexpand-1 '(define-decision-tree and-tree
                                  :value
                                  '(:true :false)
                                  
                                  '(:a (:true :false) :b (:true :false))

                                  '((:value :true :a :true :b :true)
                                    (:value :false :a :false :b :true)
                                    (:value :false :a :true :b :false)
                                    (:value :false :a :false :b :false)))))
(define-decision-tree and-tree
    :value
  '(:true :false)
                                  
  '(:a (:true :false) :b (:true :false))

  '((:value :true :a :true :b :true)
    (:value :false :a :false :b :true)
    (:value :false :a :true :b :false)
    (:value :false :a :false :b :false)))

;;; decision tree for or
(macroexpand-1 '(define-decision-tree or-tree
                 :value
                 '(:true :false)

                 '(:a (:true :false) :b (:true :false))

                 '((:value :true :a :true :b :true)
                   (:value :true :a :true :b :false)
                   (:value :true :a :false :b :true)
                   (:value :false :a :false :b :false))))

;;; decision tree for xor
(format t "~a~%" (macroexpand-1 '(define-decision-tree xor-tree
                                  :value
                                  '(:true :false)

                                  '(:a (:true :false) :b (:true :false))

                                  '((:value :false :a :true :b :true)
                                    (:value :true :a :true :b :false)
                                    (:value :false :a :false :b :false)
                                    (:value :true :a :false :b :true)))))

(define-decision-tree xor-tree
    :value
  '(:true :false)

  '(:a (:true :false) :b (:true :false))

  '((:value :false :a :true :b :true)
    (:value :true :a :true :b :false)
    (:value :false :a :false :b :false)
    (:value :true :a :false :b :true)))

;; (xor-tree '(:a :true :b :true))

(decision-tree::most-common-value :parity '((:value 2 :parity :even)
                                            (:value 5 :parity :odd)
                                            (:value 3 :parity :odd)))

(decision-tree::entropy :parity '(:even :odd) '((:value 2 :parity :even)
                                                (:value 2 :parity :even)))

(decision-tree::proportion :parity :odd '((:value 2 :parity :even)
                                          (:value 2 :parity :even)))

(decision-tree::proportion :parity :even '((:value 2 :parity :even)
                                           (:value 3 :parity :odd)))

(decision-tree::entropy-gains :parity
                              '(:even :odd)
                              '(:value (2 3 5) :max-likes (:yes :no))
                              '((:value 2 :parity :even :max-likes :yes)
                                (:value 5 :parity :odd :max-likes :yes)
                                (:value 3 :parity :odd :max-likes :no)))

(decision-tree::best-attribute :parity
                               '(:even :odd)
                               '(:value (2 3 5) :max-likes (:yes :no))
                               '((:value 2 :parity :even :max-likes :yes)
                                 (:value 5 :parity :odd :max-likes :yes)
                                 (:value 3 :parity :odd :max-likes :no)))

(defun main ()
  (sb-thread:make-thread (lambda ()
                           (swank:create-server :port 5000 :dont-close t)))
  (loop
     for input = (read)
     do (progn (format t "~a~%" (xor-tree input))
               (finish-output))))

(save-lisp-and-die "main" :executable t :toplevel #'main)
