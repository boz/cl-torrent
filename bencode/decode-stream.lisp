(in-package :cl-torrent.bencode)

(defclass decode-stream ()
  ((buffer :accessor decode-stream-buffer :initarg :buffer)
   (offset :accessor decode-stream-offset :initform 0)))

(defun have-space-p (stream size)
  (assert (>= size 0))
  (<= (+ (decode-stream-offset stream) size) (length (decode-stream-buffer stream))))

(defmacro with-space-ensured ((stream size eof-error-p eof-value) &body body)
  `(if (have-space-p ,stream ,size)
       (progn ,@body)
       (if ,eof-error-p
           (error "end of stream")
           ,eof-value)))

(defun peek-character (stream &optional peek-type eof-error-p eof-value)
  (declare (ignore peek-type))
  (with-space-ensured (stream 1 eof-error-p eof-value)
    (code-char (aref (decode-stream-buffer stream) (decode-stream-offset stream)))))

(defun read-character (stream &optional peek-type eof-error-p eof-value)
  (declare (ignore peek-type))
  (with-space-ensured (stream 1 eof-error-p eof-value)
    (with-accessors ((o decode-stream-offset) (b decode-stream-buffer)) stream
      (incf o)
      (code-char (aref b (1- o))))))

(defun read-buffer (stream size &optional eof-error-p eof-value)
  (with-space-ensured (stream size eof-error-p eof-value)
    (let* ((start (decode-stream-offset stream))
           (end   (+ start size)))
      (setf (decode-stream-offset stream) end)
      (subseq (decode-stream-buffer stream) start end))))

(defun make-decode-stream (buffer)
  (make-instance 'decode-stream :buffer (ensure-octets buffer)))
