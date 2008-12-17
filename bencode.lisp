(in-package :cl-torrent.bencode)

(defun read-next-char (sin)
  (let (c (read-byte sin nil :eof)
          (if (= :eof c)
              :eof
              (code-char c)))))

(defun skip-space (sin)
  (do ((c (read-next-char sin)
          (read-next-char sin)))
      ((not (= #\space c)) c)))

(defun decode-dispatch (c sin)
  (cond
    ((eq    c :eof)   nil)
    ((char= c #\d)    (decode-dict    c sin))
    ((char= c #\l)    (decode-list    c sin))
    ((char= c #\i)    (decode-integer c sin))
    ((digit-char-p c) (decode-string  c sin))))

(defun decode ()
  (let ((c (skip-space sin)))
    (decode-dispatch c sin)))

(defun decode-string  (c stream)
  )

(defun decode-integer (c stream))

(defun decode-dict    (c stream)
  (let (h (make-hash-table))
    h))

(defun decode-list (c stream)
  (let (lst)
    (do ((c (skip-space sin) (skip-space sin)))
        ((or (= c :eof) (char= c #\e))
         lst)
      (let ((x (decode-dispatch c sin)))
        (when x 
          (setf lst (append lst x)))))))
