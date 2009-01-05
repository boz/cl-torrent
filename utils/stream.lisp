(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.utils)

(defvar *stream-buffer-size* 8192)

(defun copy-stream-to-buffer
    (in &key (max-len (* 4 *stream-buffer-size*)) (signal-error? t))
  (let* ((element-type (stream-element-type in))
         (out (make-in-memory-output-stream :element-type element-type))
         (total 0)
         (buf (make-array *stream-buffer-size* :element-type element-type)))
    (loop
       (let ((pos (read-sequence buf in)))
         (when (zerop pos) (return (get-output-stream-sequence out)))
         (incf total pos)
         (when (and max-len (> total max-len))
           (when signal-error?
             (error "tried to read too many bytes"))
           (let ((overflow (- total max-len)))
             (decf total overflow)
             (decf pos   overflow)))
         (write-sequence buf out :end pos)
         (when (and max-len (= max-len total))
           (return (get-output-stream-sequence out)))))))
