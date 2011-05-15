(in-package :cl-torrent.file-store)

(defgeneric write-range (writer offset seq))
(defgeneric read-range  (reader offset size))

(defclass protected-range-mixin ()
  ((mutex  :reader mutex  :initarg :mutex :initform (make-mutex))))

(defmethod write-range :around ((writer protected-stream) offset seq)
  (with-mutex ((mutex writer))
    (call-next-method)))

(defmethod read-range :around ((reader protected-stream) offset size)
  (with-mutex ((mutex writer))
    (call-next-method)))

(defclass protected-range-stream (protected-range-mixin)
  ((stream :reader stream :initarg :stream)))

(defmethod write-range ((writer protected-range-stream) offset seq)
  (assert (>= offset 0))
  (when (file-position (stream writer) offset)
    (write-sequence seq (stream writer))))

(defmethod read-range ((reader protected-range-stream) offset size)
  (assert (>= offset 0))
  (with-accessors (stream) reader
    (when (file-position stream offset)
      (let ((seq (make-array size :element-type)))
        (read-sequence seq stream)
        seq))))

(defun make-protected-range-stream (stream)
  (assert stream)
  (make-instance 'protected-range-stream :stream stream))

(defclass offset-stream-wrapper ()
  ((stream :reader stream :initarg :stream)
   (base   :reader base   :initarg :base :initform 0)
   (size   :reader size   :initarg :size :initform nil)))

(defun make-offset-range-stream (stream &optional &key (base 0) (size nil))
  (make-instance 'offset-stream-wrapper :stream stream :base base :size size))

(defmethod write-range ((writer offset-stream-wrapper) offset seq)
  (with-accessors (base size stream) writer
    (when size
      (assert (>= size (+ offset (length seq)))))
    (write-range stream (+ base offset) seq)))

(defmethod read-range ((reader offset-stream-wrapper) offset len)
  (with-accessors (base size stream) reader
    (when size
      (assert (>= size (+ offset len))))
    (read-range stream (+ base offset) len)))
