(defpackage :decision-tree
  (:use :cl :utils)
  (:export define-decision-tree))

(in-package #:decision-tree)

(defvar *tree-definition-list* '())

(defun attribute-eq-func (attribute value &optional (p #'eq))
  "Returns a function that determines whether or not an instance
has value for attribute."
  #'(lambda (item)
      (funcall p (getf item attribute) value)))

(defun proportion (attribute value sample-space)
  "Returns the proportion of items in sample-space that have 
attribute eq to value."
  (/ (length (utils:filter sample-space
                           (attribute-eq-func attribute value)))
     (length sample-space)))

(defun entropy (attribute possible-values sample-space)
  "Returns the entropy of attribute in the sample space"
  (loop for value in possible-values
     for proportion = (proportion attribute value sample-space)
     when (not (zerop proportion))
     summing (* (- proportion) (log proportion 2))))

(defun entropy-gain
    (attribute possible-values target-attribute target-values
     sample-space)
  "Returns the entropy gain metric for attribute in sample-space"
  (let ((s-length (length sample-space)))
    (- (entropy target-attribute target-values sample-space)
       (loop for value in possible-values
          for subspace = (utils:filter sample-space
                                 (attribute-eq-func attribute value))
          when subspace
          summing
            (* (/ (length subspace) s-length)
               (entropy target-attribute target-values subspace))))))

(defun most-common-value (attribute examples)
  "Returns the most common value of attribute among examples"
  (utils:hash-maximum
   (let ((hash (make-hash-table)))
     (progn (loop for ex in examples
               do (setf (gethash (getf ex attribute) hash)
                        (1+ (gethash (getf ex attribute) hash 0)))))
     hash)))

(defun equal-sample-space-p (attribute space)
  "If every value in space for attribute is the same, return that value.
Otherwise, nil is returned."
  (loop for instance in space
     for val = (getf instance attribute) then val
     when (not (eq val (getf instance attribute)))
     do (return nil)
     finally (return val)))

(defun entropy-gains (target target-values attribute-seq sample-space)
  "Returns a sequence of attributes and their respective entropy gains."
  (loop for (attribute-name . (attribute-values)) on attribute-seq by #'cddr
     when (not (eq attribute-name target))
     collecting attribute-name
     and
     collecting
       (entropy-gain
        attribute-name attribute-values target target-values sample-space)))

(defun best-attribute (target values attributes-seq sample-space)
  "Returns the best attribute to use as a test."
  (let ((entropy-gains (entropy-gains target values attributes-seq sample-space)))
    (loop for (attribute . (entropy-gain)) on entropy-gains by #'cddr
       maximizing entropy-gain into max-gain
       finally (return
                 (loop for (attribute . (entropy-gain)) on entropy-gains by #'cddr
                    when (= entropy-gain max-gain)
                    do (return attribute))))))

(defun remove-attribute (attribute attribute-seq)
  (loop for (name . (values)) on attribute-seq by #'cddr
     when (not (eq name attribute))
     collecting name and collecting values))

(defun make-decision-tree (target-attribute target-values
                           attributes-and-values sample-space p-name)
  "target-attribute is the attribute that we would like to be able to classify for. 
target-values is a sequence of possible values that this attribute can be.
attributes-and-values is a plist in the form (attribute value-list ...)
sample-space gives the training data as ((attribute1 val attribute2 val2 ...) ...)"
  (let ((equal-sample-space-result
         (equal-sample-space-p target-attribute sample-space)))
    (cond (equal-sample-space-result
           equal-sample-space-result)
          
          ((null attributes-and-values)
           (most-common-value target-attribute sample-space))
          
          (t (let ((a (best-attribute target-attribute
                                      target-values
                                      attributes-and-values
                                      sample-space)))
               (cons 'cond
                     (loop for value in (getf attributes-and-values a)
                        for examples = 
                          (utils:filter sample-space (attribute-eq-func a value))
                        collecting
                          `((eq (getf ,p-name ,a) ,value)
                            ,(if (null examples)
                                 (most-common-value target-attribute sample-space)
                                 (make-decision-tree target-attribute
                                                     target-values
                                                     (remove-attribute
                                                      a attributes-and-values)
                                                     examples
                                                     p-name))))))))))

(defmacro define-decision-tree (name target-attribute target-values
                                attribute-seq sample-space)
  (let ((decision-tree (make-decision-tree 
                        (eval target-attribute)
                        (eval target-values)
                        (eval attribute-seq)
                        (eval sample-space)
                        'item)))
    `(progn (defun ,name (item)
              ,decision-tree)
            (defparameter ,name (quote ,decision-tree)))))
