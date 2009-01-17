(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.bencode)

(defgeneric decode-bencmap (type dict))
(defgeneric decode-bencmap-value (type value dict))

(defmacro with-bencode-value ((name obj) &body body)
  `(when ,obj
     (let ((,name (bencode-object-value ,obj)))
       ,@body)))

(defmethod decode-bencmap-value ((type (eql :string)) value dict)
  (declare (ignore type dict))
  (when value
    (bencode-string-value value)))

(defmethod decode-bencmap-value ((type (eql :integer)) value dict)
  (declare (ignore type dict))
  (with-bencode-value (value value)
    (assert (integerp value))
    value))

(defmethod decode-bencmap-value ((type (eql :epoch-date)) value dict)
  (declare (ignore type dict))
  (with-bencode-value (value value)
    (assert (integerp value))
    value))

(defmethod decode-bencmap-value ((type (eql :string-list)) value dict)
  (declare (ignore type dict))
  (with-bencode-value (value value)
    (assert (listp value))
    (mapcar #'bencode-string-value value)))

(defgeneric bencmap->list (obj))
(defmethod bencmap->list ((obj t)) obj)
(defmethod bencmap->list ((obj list))
  (mapcar #'bencmap->list obj))

(defun bencmap-key-spec->slot (spec)
  (let ((name (first spec)))
    (list name :accessor name :initarg (symbol->keyword name))))

(defun generate-slot-decoder (dict slot)
  (destructuring-bind (name type &key (required nil) (key nil)) slot
    (let ((key (or key (string-downcase (symbol-name name))))
          (val (gensym "dict-value-")))
      `(let ((,val (bencode-dictionary-get ,dict ,key)))
         ,@(if required `((assert ,val)))
         (setf ,name
               (decode-bencmap-value ,type ,val ,dict))))))

(defmacro generate-bencmap-decoder (name slots)
  (with-gensyms (obj dict)
    `(defmethod decode-bencmap ((type (eql ',name)) ,dict)
       (let ((,obj (make-instance ',name)))
         (with-slots ,(mapcar #'first slots) ,obj
           ,@(mapcar #'(lambda (x) (generate-slot-decoder dict x)) slots))
         ,obj))))

(defmacro generate-bencmap->list (name slots)
  (with-gensyms (obj)
    (let ((slots (mapcar #'first slots)))
      `(defmethod bencmap->list ((,obj ,name))
         (list ,(symbol->keyword name)
               ,@(loop
                    for x in slots
                    collect (symbol->keyword x)
                    collect `(bencmap->list (slot-value ,obj ',x))))))))

(defmacro generate-bencmap-class (name superclasses slots)
  `(defclass ,name ,superclasses
     ,(mapcar #'bencmap-key-spec->slot slots)))

(defmacro define-bencmap-class (name superclasses slots)
  `(progn
     (generate-bencmap-class    ,name ,superclasses ,slots)
     (generate-bencmap->list    ,name ,slots)
     (generate-bencmap-decoder  ,name ,slots)))

(defun bencmap-decode (buffer type)
  (let ((obj (bencode-decode buffer)))
    (decode-bencmap type obj)))

(defun bencmap-decode-file (pathname type)
  (let ((obj (bencode-decode-file pathname)))
       (decode-bencmap type obj)))
