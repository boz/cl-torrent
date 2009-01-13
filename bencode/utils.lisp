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
       (c (peek-character stream t nil) (peek-character stream t nil)))
      ((not (digit-char-p c)) x)
    (setf x (+ (char->number (read-character stream))
               (* 10 x)))))

(defun octets->string (buf)
  (octets-to-string buf :external-format *bencode-external-string-format*))

(defun string->octets (buf)
  (string-to-octets buf :external-format *bencode-external-string-format*))

(defun ensure-octets (obj)
  (etypecase obj
    (string (string->octets obj))
    ((array (unsigned-byte 8)) obj)
    (simple-vector obj)))
