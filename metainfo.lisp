
;; (declaim (optimize (speed 0) (safety 3) (debug 3)))

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

(defmethod decode-bencmap-value ((type (eql 'info-hash)) value ctx)
  (declare (ignore type))
  (assert (not value))
  (multiple-value-bind (bytes exists)
      (decode-ctx-get-bytes ctx "info")
    (assert exists)
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 bytes))))

(defmethod decode-bencmap-value ((type (eql 'announce-list)) value ctx)
  "http://www.bittornado.com/docs/multitracker-spec.txt"
  (declare (ignore type ctx))
  (when value
    (assert (listp value))
    (flet ((f (l) (mapcar #'octets->string l)))
      (mapcar #'f value))))

(defmethod decode-bencmap-value ((type (eql 'epoch-date)) value ctx)
  (declare (ignore type ctx))
  (when value
    (assert (integerp value))
    value))

(defmethod decode-bencmap-value ((type (eql 'info-dict)) value ctx)
  (declare (ignore type))
  (when value
    (assert (hash-table-p value))
    (let ((ctx (new-decode-ctx ctx value)))
      (if (decode-ctx-contains ctx "files")
          (decode-bencmap 'info-dict-multi ctx)
          (decode-bencmap 'info-dict-single ctx)))))

(defmethod decode-bencmap-value ((type (eql 'piece-list)) value ctx)
  (declare (ignore type ctx))
  (when value
    (loop
       repeat (/ (length value) 20)
       for x = 0 then (+ x 20)
       collecting (subseq value x (+ x 20)))))

(defmethod decode-bencmap-value ((type (eql 'info-file-list)) value ctx)
  (declare (ignore type))
  (when value
    (assert (listp value))
    (flet ((do-decode (x)
             (decode-bencmap 'info-dict-multi-file
                             (new-decode-ctx ctx x))))
      (mapcar #'do-decode value))))

(defmethod decode-bencmap-value ((type (eql 'string-list)) value ctx)
  (declare (ignore type ctx))
  (when value
    (assert (listp value))
    (mapcar #'octets->string value)))

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop
     for super in (get name 'superclasses)
     nconc (direct-slots super)
     nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

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

(defmacro define-class-bencmap->list (name)
  (with-gensyms (obj)
    (let ((slots (mapcar #'first (all-slots name))))
      `(defmethod bencmap->list ((,obj ,name))
         (list ,(symbol->keyword name)
               ,@(loop
                    for x in slots
                    collect (symbol->keyword x)
                    collect `(bencmap->list (slot-value ,obj ',x))))))))

(defmacro defbencmap (name superclasses slots)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'slots) ',slots)
       (setf (get ',name 'superclasses) ',superclasses))
     (defclass ,name ,superclasses
       ,(mapcar #'(lambda (x)
                    (let ((name (first x)))
                      (list name :accessor name :initarg (symbol->keyword
                                                          name)))) slots))
     (define-class-bencmap->list ,name)
     (define-class-bencmap-decoder ,name ,(all-slots name))))

(defbencmap metainfo ()
  ((info          'info-dict  :required t)
   (announce      'string     :required t)
   (creation-date 'epoch-date :key "creation date")
   (created-by    'string     :key "created by")
   (info-hash     'info-hash)
   (announce-list 'announce-list)
   (comment       'string)))

(defbencmap info-dict-single ()
  ((piece-length 'integer    :required t :key "piece length")
   (byte-length  'integer    :required t :key "length")
   (pieces       'piece-list :required t)
   (name         'string     :required t)
   (private      'integer)
   (md5sum       'string)))

(defbencmap info-dict-multi ()
  ((piece-length 'integer        :required t :key "piece length")
   (pieces       'piece-list     :required t)
   (files        'info-file-list :required t)
   (name         'string         :required t)
   (private      'integer)))

(defbencmap info-dict-multi-file ()
  ((byte-length 'integer     :required t :key "length")
   (path        'string-list :required t)
   (md5sum      'string)))

(defgeneric is-multi-file (info))
(defmethod  is-multi-file ((info info-dict-multi))  t)
(defmethod  is-multi-file ((info info-dict-single)) nil)

(defun metainfo-decode (obj)
  (multiple-value-bind (benc-hash range-map byte-buffer)
      (bencode-decode obj t)
    (decode-bencmap 'metainfo
                    (make-instance 'bencmap-decode-ctx
                                   :benc-hash   benc-hash
                                   :range-map   range-map
                                   :byte-buffer byte-buffer))))

(defun metainfo-decode-file (pathname)
  "Decode the file named ``pathname''."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (metainfo-decode stream)))
