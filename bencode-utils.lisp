(in-package :cl-torrent)

(defun char->number (c)
  (- (char-code c)
     (char-code #\0)))

(defun buf->string (buf)
  (map 'string #'code-char buf))

(defun read-next-char (sin &optional (return-eof nil))
  (if return-eof
      (let ((c (read-byte sin nil :eof)))
        (if (eq :eof c)
            :eof
            (code-char c)))
      (code-char (read-byte sin))))

(defun slurp-decimal (sin &optional (c #\0))
  (do ((x 0)
       (c c (read-next-char sin)))
      ((not (digit-char-p c))
       (values x c))
    (setf x (+ (char->number c)
               (* 10 x)))))
