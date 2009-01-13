(in-package :cl-torrent.bencode)

(defclass decode-stream ()
  ((buffer :accessor buffer :initarg :buffer)
   (offset :accessor offset :initform 0)))

(defun have-space-p (stream size)
  (assert (>= size 0))
  (<= (+ (offset stream) size) (length (buffer stream))))

(defmacro with-space-ensured ((stream size eof-error-p eof-value) &body body)
  `(if (have-space-p ,stream ,size)
       (progn ,@body)
       (if ,eof-error-p
           (error "end of stream")
           ,eof-value)))

(defun peek-character (stream &optional peek-type eof-error-p eof-value)
  (declare (ignore peek-type))
  (with-space-ensured (stream 1 eof-error-p eof-value)
    (code-char (aref (buffer stream) (offset stream)))))

(defun read-character (stream &optional peek-type eof-error-p eof-value)
  (declare (ignore peek-type))
  (with-space-ensured (stream 1 eof-error-p eof-value)
    (with-accessors ((o offset) (b buffer)) stream
      (incf o)
      (code-char (aref b (1- o))))))

(defun read-buffer (stream size &optional eof-error-p eof-value)
  (with-space-ensured (stream size eof-error-p eof-value)
    (let* ((start (offset stream))
           (end   (+ start size)))
      (setf (offset stream) end)
      (subseq (buffer stream) start end))))

(defun make-decode-stream (buffer)
  (make-instance 'decode-stream :buffer buffer))
