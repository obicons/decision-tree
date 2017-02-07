(defpackage :utils
  (:use :cl)
  (:export filter
           hash-keys
           hash-maximum))

(in-package :utils)

(defun filter (seq predicate)
  "Returns all items in seq that satisfy predicate"
  (loop for item in seq
     when (funcall predicate item) collect item))

(defun hash-keys (hash-table)
  "Returns a sequence of hash-table keys"
  (loop for key being the hash-keys of hash-table
     collecting key))

(defun hash-maximum (hash-table &key
                                  (keys (hash-keys hash-table))
                                  (max-key (first keys)))
  "Returns the key in hash-table with the largest associated value."
  (if (null keys)
      max-key
      (hash-maximum hash-table
                    :keys (rest keys)
                    :max-key (if (> (gethash (first keys) hash-table)
                                    (gethash max-key hash-table))
                                 (first keys)
                                 max-key))))
