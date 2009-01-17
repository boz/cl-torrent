(in-package :cl-torrent.bencode)

(defgeneric bencode-object= (x y))

(defclass bencode-object ()
  ((bytes :accessor bencode-object-bytes :initarg :bytes :initform nil)
   (value :accessor bencode-object-value :initarg :value :initform nil)))

(defclass bencode-string (bencode-object) ())
(defclass bencode-integer (bencode-object) ())
(defclass bencode-dictionary (bencode-object) ())
(defclass bencode-list (bencode-object) ())

(defmethod bencode-object= ((x bencode-object) (y bencode-object))
  (equalp (bencode-object-value x) (bencode-object-value y)))

(defmethod bencode-object= ((obj bencode-string) y)
  (call-next-method obj (ensure-bencode-string y)))

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

(defun bencode-string-value (obj)
  (octets->string (bencode-object-value obj)))

(defun make-bencode-string (str)
  (ensure-bencode-string str))
(defun make-bencode-integer (&optional value)
  (make-instance 'bencode-integer :value value))
(defun make-bencode-dictionary ()
  (make-instance 'bencode-dictionary))
(defun make-bencode-list (&optional value)
  (make-instance 'bencode-list :value value))
