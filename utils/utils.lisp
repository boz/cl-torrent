(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.utils)

(defun generate-random-bytes (len)
  (let ((buf (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len buf)
      (setf (aref buf i) (random #xff)))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun symbol->keyword (sym)
  (intern (symbol-name sym) 'keyword))

