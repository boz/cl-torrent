(in-package :cl-torrent.bencode)

(defgeneric bencode-object= (obj &rest rest))

(defclass bencode-object ()
  ((bytes :accessor bencode-object-bytes :initarg :bytes :initform nil)
   (value :accessor bencode-object-value :initarg :value :initform nil)))

(defclass bencode-string (bencode-object) ())
(defclass bencode-integer (bencode-object) ())
(defclass bencode-dictionary (bencode-object) ())
(defclass bencode-list (bencode-object) ())

(defmethod bencode-object= ((obj bencode-object) &rest rest)
  (apply #'equalp (bencode-object-value obj) rest))

(defmethod bencode-object= ((obj bencode-string) &rest rest)
  (flet ((xform (x)
           (if (typep x 'string) (string->octets x) x)))
    (apply #'equalp (bencode-object-value obj) (mapcar #'xform rest))))

(defun ensure-bencode-string (obj)
  (etypecase obj
    (string
     (make-instance 'bencode-string :value (string->octets obj)))
    ((array (unsigned-byte 8))
     (make-instance 'bencode-string :value obj))
    (bencode-string obj)))

(defmacro with-dict-lookup ((key alist cell) dict &body body)
  `(let* ((,key (ensure-bencode-string ,key))
          (,alist (bencode-object-value ,dict))
          (,cell  (assoc ,key ,alist :test #'bencode-object=)))
     ,@body))

(defun bencode-dictionary-set (dict key val)
  (with-dict-lookup (key alist cell) dict
    (if cell
        (setf (cdr cell) key)
        (setf (bencode-object-value dict) (acons key val alist)))))

(defun bencode-dictionary-get (dict key)
  (with-dict-lookup (key alist cell) dict
    (when cell (cdr cell))))