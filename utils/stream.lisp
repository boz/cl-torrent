(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.utils)

(defvar *stream-buffer-size* 8192)

(defun copy-stream-to-buffer-bound (in out buf max-len signal-error?)
  (let ((total 0))
    (loop
       (when (> total max-len)
         (if signal-error?
             (error "tried to read to much")
             (return (get-output-stream-sequence out))))
       (let ((pos (read-sequence buf in)))
         (when (zerop pos) (return (get-output-stream-sequence out)))
         (incf total pos)
         (if (> total max-len)
             (decf pos (- total max-len)))
         (write-sequence buf out :end pos)))))

(defun copy-stream-to-buffer
    (in &key (max-len (* 32 *stream-buffer-size*)) (signal-error? t))
  (let* ((element-type (stream-element-type in))
         (out (make-in-memory-output-stream :element-type element-type))
         (buf (make-array *stream-buffer-size* :element-type element-type)))
    (if max-len
        (copy-stream-to-buffer-bound in out buf max-len signal-error?)
        (loop
           (let ((pos (read-sequence buf in)))
             (when (zerop pos) (return (get-output-stream-sequence out)))
             (write-sequence buf out :end pos))))))

