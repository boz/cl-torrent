(in-package :cl-torrent.bencode)

(defparameter *bencode-external-format*
  (make-external-format :ASCII))

(defparameter *bencode-external-string-format*
  (make-external-format :UTF-8))

(defun char->number (c)
  (- (char-code c)
     (char-code #\0)))

(defun read-decimal (stream)
  (do ((x 0)
       (c (peek-char nil stream) (peek-char nil stream)))
      ((not (digit-char-p c)) x)
    (setf x (+ (char->number (read-char stream))
               (* 10 x)))))

(defun octets->string (buf)
  (octets-to-string buf :external-format *bencode-external-string-format*))

(defun string->octets (buf)
  (string-to-octets buf :external-format *bencode-external-string-format*))

(defun ensure-flexi-stream (stream &optional (external-format :ascii))
  (etypecase stream
    (flexi-stream stream)
    (stream (make-flexi-stream stream :external-format external-format))
    (string (ensure-flexi-stream
             (string-to-octets stream :external-format external-format)
             external-format))
    (list (ensure-flexi-stream
           (make-in-memory-input-stream stream)
           external-format))
    (vector (ensure-flexi-stream
             (make-in-memory-input-stream stream)
             external-format))))

(defmacro with-return-val ((val-name val-expr) &body body)
  `(let ((,val-name ,val-expr))
     (progn ,@body)
     ,val-name))
