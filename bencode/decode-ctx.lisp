(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent)

(defclass bencmap-decode-ctx ()
  ((benc-hash   :reader benc-hash   :initarg :benc-hash)
   (range-map   :reader range-map   :initarg :range-map)
   (byte-buffer :reader byte-buffer :initarg :byte-buffer)))

(defun new-decode-ctx (old-ctx new-hash)
  (make-instance 'bencmap-decode-ctx
                 :benc-hash   new-hash
                 :range-map   (range-map old-ctx)
                 :byte-buffer (byte-buffer old-ctx)))

(defun decode-ctx-get (ctx key)
  (gethash key (benc-hash ctx)))

(defun decode-ctx-contains (ctx key)
  (multiple-value-bind (val exists) (decode-ctx-get ctx key)
    (declare (ignore val))
    exists))

(defun decode-ctx-get-range (ctx key)
  (multiple-value-bind (obj exists) (decode-ctx-get ctx key)
    (if (not exists)
        (values nil nil)
        (gethash obj (range-map ctx)))))

(defun decode-ctx-get-bytes (ctx key)
  (multiple-value-bind (range exists) (decode-ctx-get-range ctx key)
    (if (not exists)
        (values nil nil)
        (values
         (subseq (byte-buffer ctx) (first range) (cdr range)) t))))

