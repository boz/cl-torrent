(in-package :cl-torrent)

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