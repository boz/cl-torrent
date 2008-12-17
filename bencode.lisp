

(in-package :cl-torrent)

(defun decode (sin)
  (let ((c (read-next-char sin t)))
    (decode-dispatch c sin)))

(defun decode-dispatch (c sin)
  (cond
    ((eq    c :eof)   nil)
    ((char= c #\d)    (decode-dict    c sin))
    ((char= c #\l)    (decode-list    c sin))
    ((char= c #\i)    (decode-integer c sin))
    ((digit-char-p c) (decode-string  c sin))))

(defun decode-string (c sin)
  (assert (digit-char-p c))
  (multiple-value-bind (len c)
      (slurp-decimal sin c)
    (assert (char= #\: c))
    (let* ((buf (make-array len))
           (rlen (read-sequence buf sin)))
      (assert (= rlen len))
      buf)))

(defun decode-integer (c sin)
  (assert (char= c #\i))
  (multiple-value-bind (x c)
      (slurp-decimal sin)
    (assert (char= #\e c))
    x))

(defun decode-dict (c sin)
  (assert (char= c #\d))
  (do ((c (read-next-char sin nil)
          (read-next-char sin nil))
       (dict (make-hash-table :test #'equal)))
      ((char= c #\e) dict)
    (assert (digit-char-p c))
    (let ((key (buf->string (decode-dispatch c sin)))
          (val (decode-dispatch (read-next-char sin nil) sin)))
      (setf (gethash key dict) val))))

(defun decode-list (c sin)
  (assert (char= c #\l))
  (do ((c (read-next-char sin nil)
          (read-next-char sin nil))
       (lst nil))
      ((char= c #\e) (nreverse lst))
    (setf lst
          (cons (decode-dispatch c sin) lst))))
