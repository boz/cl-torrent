(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun symbol->keyword (sym)
  (intern (symbol-name sym) 'keyword))
