(in-package :cl-torrent)

(defclass flexi-echo-stream (flexi-input-stream)
  ((flexi-echo-stream-output-stream
    :initarg :output-stream :accessor flexi-echo-stream-output-stream)))

(defmethod stream-read-byte :around ((stream flexi-echo-stream))
  (with-return-val (x (call-next-method))
    (unless (eq x :eof)
      (write-byte x (flexi-stream-stream stream)))))

(defun make-flexi-echo-stream (input-stream &key (external-format :ascii))
  (let ((in (ensure-flexi-stream input-stream external-format))
        (out (make-in-memory-output-stream)))
    (make-instance 'flexi-echo-stream
                   :stream in
                   :output-stream out
                   :element-type  (stream-element-type in)
                   :flexi-stream-external-format
                   (flexi-stream-external-format in))))

(define-test ensure-flexi-stream
  (macrolet ((assert-stream (obj)
               `(assert-true
                 (typep (ensure-flexi-stream ,obj) 'flexi-input-stream))))
    (assert-stream "foo")
    (assert-stream *standard-input*)
    (assert-stream #(10 11 12))
    (assert-stream '(1 2 3))))
