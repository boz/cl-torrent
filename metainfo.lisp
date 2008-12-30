(in-package :cl-torrent)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; (defgeneric bencmap-read-object (type ))
;; (defclass bencmap-base () ())
;; (defun slot->defclass-slot (spec))

;; (defmacro defbencmap (name slots)
;;   `(defclass ,name (bencmap-base)
;;      (,(mapcar #'slot->defclass-slot slots))))

;; (defbencmap metainfo
;;     ((info          info-dict   :required t)
;;      (announce      string      :required t)
;;      (announce-list string-list :required t)
;;      (creation-date epoch-date  :key "creation date")
;;      (created-by    string      :key "created by")
;;      (info-hash     info-hash)
;;      (comment       string)))

(defclass metainfo ()
  ((info
    :accessor info :initarg :info)
   (info-hash
    :accessor info-hash :initarg :info-hash)
   (announce
    :accessor announce :initarg :announce)
   (announce-list
    :accessor announce-list :initarg :announce-list :initform nil)
   (creation-date
    :accessor creation-date :initarg :creation-date :initform nil)
   (comment
    :accessor comment :initarg :comment :initform nil)
   (created-by
    :accessor created-by :initarg :created-by :initform nil)))

(defclass info-dict ()
  ((piece-length
    :accessor piece-length-of :initarg :piece-length)
   (pieces
    :accessor pieces :initarg :pieces)
   (private
    :accessor private :initarg :private :initform 1)))

(defclass info-dict-single (info-dict)
  ((name
    :accessor name :initarg :name :initform nil)
   (length
    :accessor length-of :initarg length)
   (md5sum
    :accessor md5sum :initarg :md5sum :initform nil)))

(defclass info-dict-multi (info-dict)
  ((name
    :accessor name :initarg :name :initform nil)
   (files
    :accessor files :initarg :files)))

(defclass info-dict-multi-file ()
  ((length
    :accessor length-of :initarg :length)
   (md5sum
    :accessor md5sum :initarg :md5sum :initform nil)
   (path
    :accessor path :initarg :path)))

(defun getstring (key hash)
  (let ((val (gethash key hash)))
    (if (and val (length val))
        (octets->string val)
        nil)))

(defun parse-announce-list (lst)
  "http://www.bittornado.com/docs/multitracker-spec.txt"
  (flet ((f (l)
           (mapcar #'octets->string l)))
    (mapcar #'f lst)))

(defun make-info-hash (info-hash offset-map buffer)
  (let ((range (gethash info-hash offset-map)))
    (assert range)
    (let ((buffer (subseq buffer (car range) (cdr range))))
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-sequence :sha1 buffer)))))

(defun make-pieces (seq)
  (loop
     repeat (/ (length seq) 20)
     for x = 0 then (+ x 20)
     collecting (subseq seq x (+ x 20))))

(defun info-dict-single-decode (dict &rest rest)
  (let ((md5sum (getstring "md5sum" dict)))
    (apply 'make-instance
           'info-dict-single
           :md5sum md5sum
           rest)))

(defun info-dict-multi-file-decode (dict)
  (let ((len (gethash "length" dict))
        (md5sum (getstring "md5sum" dict))
        (path   (mapcar #'octets->string (gethash "path" dict))))
    (make-instance 'info-dict-multi-file
                   :length len
                   :md5sum md5sum
                   :path   path)))

(defun info-dict-multi-decode (dict &rest rest)
  (let* ((files (gethash "files" dict))
         (files (mapcar #'info-dict-multi-file-decode files)))
    (apply #'make-instance
           'info-dict-multi
           :files files
           rest)))

(defun info-dict-decode (dict)
  (let* ((piece-length (gethash "piece length" dict))
         (pieces (make-pieces (gethash "pieces" dict)))
         (private (gethash "private" dict))
         (name    (getstring "name" dict))
         (args (list :piece-length piece-length
                     :pieces       pieces
                     :private      private
                     :name         name)))
    (multiple-value-bind (len exists)
        (gethash "length" dict)
      (if exists
          (apply #'info-dict-single-decode dict :length len args)
          (apply #'info-dict-multi-decode dict args)))))

(defun metainfo-decode (input)
  (multiple-value-bind (val map buf)
      (bencode-decode input t)
    (let* ((info
            (gethash "info" val))
           (info-hash
            (make-info-hash info map buf))
           (info (info-dict-decode info))
           (announce
            (getstring "announce" val))
           (announce-list
            (parse-announce-list (gethash "announce-list" val)))
           (creation-date
            (gethash "creation date" val))
           (comment
            (getstring "comment" val))
           (created-by
            (getstring "created-by" val)))
      (make-instance 'metainfo
                     :info info
                     :info-hash info-hash
                     :announce announce
                     :announce-list announce-list
                     :creation-date creation-date
                     :comment comment
                     :created-by created-by))))

(defun metainfo-decode-file (pathname)
  "Decode the file named ``pathname''."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (metainfo-decode stream)))
