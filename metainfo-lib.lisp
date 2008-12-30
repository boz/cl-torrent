(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :cl-torrent)

(defclass bencmap-decode-ctx ()
  ((benc-hash   :reader benc-hash   :initarg :benc-hash)
   (range-map   :reader range-map   :initarg :range-map)
   (byte-buffer :reader byte-buffer :initarg :byte-buffer)))

(defun new-decode-ctx (old-ctx new-hash)
  (make-instance 'bencmap-decode-ctx
                 :benc-hash   new-hash
                 :range-map   (range-map old-ctx)
                 :byte-buffer (byte-buffer old-ctx)))

(defun decode-ctx-get (ctx key)
  (gethash key (benc-hash ctx)))

(defun decode-ctx-contains (ctx key)
  (multiple-value-bind (val exists) (decode-ctx-get ctx key)
    (declare (ignore val))
    exists))

(defun decode-ctx-get-range (ctx key)
  (multiple-value-bind (obj exists) (decode-ctx-get ctx key)
    (if (not exists)
        (values nil nil)
        (gethash obj (range-map ctx)))))

(defun decode-ctx-get-bytes (ctx key)
  (multiple-value-bind (range exists) (decode-ctx-get-range ctx key)
    (if (not exists)
        (values nil nil)
        (values
         (subseq (byte-buffer ctx) (first range) (cdr range)) t))))

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

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun symbol->keyword (sym)
  (intern (symbol-name sym) 'keyword))

(defun bencmap-key-spec->slot (spec)
  (let ((name (first spec)))
    (list name :accessor name :initarg (symbol->keyword name))))

(defun slot-decoder (ctx slot)
  (destructuring-bind (name type &key (required nil) (key nil)) slot
    (let ((key (or key (string-downcase (symbol-name name)))))
      (with-gensyms (val exists)
        `(multiple-value-bind (,val ,exists) (decode-ctx-get ,ctx ,key)
           ,@(if required `((assert ,exists)) `((declare (ignore ,exists))))
           (setf ,name (decode-bencmap-value ,type ,val ,ctx)))))))

(defmacro define-class-bencmap-decoder (name slots)
  (with-gensyms (obj ctx)
    `(defmethod decode-bencmap ((type (eql ',name)) ,ctx)
       (let ((,obj (make-instance ',name)))
         (with-slots ,(mapcar #'first slots) ,obj
           ,@(mapcar #'(lambda (x) (slot-decoder ctx x)) slots))
         ,obj))))

(defmacro define-class-bencmap->list (name slots)
  (with-gensyms (obj)
    (let ((slots (mapcar #'first slots)))
      `(defmethod bencmap->list ((,obj ,name))
         (list ,(symbol->keyword name)
               ,@(loop
                    for x in slots
                    collect (symbol->keyword x)
                    collect `(bencmap->list (slot-value ,obj ',x))))))))

(defmacro define-bencmap-class (name superclasses slots)
  `(defclass ,name ,superclasses
     ,(mapcar #'bencmap-key-spec->slot slots)))

(defmacro defbencmap (name superclasses slots)
  `(progn
     (define-bencmap-class ,name   ,superclasses ,slots)
     (define-class-bencmap->list   ,name ,slots)
     (define-class-bencmap-decoder ,name ,slots)))
