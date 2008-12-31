(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.bencode)

(defgeneric decode-bencmap (type ctx))
(defgeneric decode-bencmap-value (type value ctx))

(defmethod decode-bencmap-value ((type (eql 'string)) value ctx)
  (declare (ignore type ctx))
  (when value
    (octets->string value)))

(defmethod decode-bencmap-value ((type (eql 'integer)) value ctx)
  (declare (ignore type ctx))
  (when value
    (assert (integerp value))
    value))

(defmethod decode-bencmap-value ((type (eql 'epoch-date)) value ctx)
  (declare (ignore type ctx))
  (when value
    (assert (integerp value))
    value))

(defmethod decode-bencmap-value ((type (eql 'string-list)) value ctx)
  (declare (ignore type ctx))
  (when value
    (assert (listp value))
    (mapcar #'octets->string value)))

(defgeneric bencmap->list (obj))
(defmethod bencmap->list ((obj t)) obj)
(defmethod bencmap->list ((obj list))
  (mapcar #'bencmap->list obj))

(defun bencmap-key-spec->slot (spec)
  (let ((name (first spec)))
    (list name :accessor name :initarg (symbol->keyword name))))

(defun generate-slot-decoder (ctx slot)
  (destructuring-bind (name type &key (required nil) (key nil)) slot
    (let ((key (or key (string-downcase (symbol-name name)))))
      (with-gensyms (val exists)
        `(multiple-value-bind (,val ,exists) (decode-ctx-get ,ctx ,key)
           ,@(if required `((assert ,exists)) `((declare (ignore ,exists))))
           (setf ,name (decode-bencmap-value ,type ,val ,ctx)))))))

(defmacro generate-bencmap-decoder (name slots)
  (with-gensyms (obj ctx)
    `(defmethod decode-bencmap ((type (eql ',name)) ,ctx)
       (let ((,obj (make-instance ',name)))
         (with-slots ,(mapcar #'first slots) ,obj
           ,@(mapcar #'(lambda (x) (generate-slot-decoder ctx x)) slots))
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

(defun bencmap-decode (obj type)
  (multiple-value-bind (benc-hash range-map byte-buffer)
      (bencode-decode obj t)
    (decode-bencmap type
                    (make-instance 'bencmap-decode-ctx
                                   :benc-hash   benc-hash
                                   :range-map   range-map
                                   :byte-buffer byte-buffer))))

(defun bencmap-decode-file (pathname type)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (bencmap-decode stream type)))
