(in-package :cl-torrent)

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
  ((length
    :accessor length-of :initarg :length)
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

(defclass metainfo-input-stream (flexi-input-stream)
  ((object-map
    :initarg :object-map :accessor object-map-of)
   (output-stream
    :initarg :output-stream :accessor output-stream-of))
  (:documentation ""))

(defun make-metainfo-input-stream (stream)
  (let* ((out
          (make-flexi-stream (make-in-memory-output-stream)
                             :external-format *bencode-external-format*))
         (stream (make-echo-stream stream out)))
    (make-instance 'metainfo-input-stream
                   :stream stream
                   :element-type 'octet
                   :flexi-stream-external-format *bencode-external-format*
                   :object-map (make-hash-table :test 'eq)
                   :output-stream (flexi-stream-stream out))))

(defclass metainfo-decoder (bencode-decoder) ()
  (:documentation
   "Decode metainfo files."))

(defmethod decode-from-stream :around ((decoder metainfo-decoder) stream)
  (let* ((stream (make-metainfo-input-stream stream))
         (val    (call-next-method decoder stream))
         (map    (object-map-of stream))
         (buf    (get-output-stream-sequence (output-stream-of stream))))
    (values val map buf)))

(defmethod decode-object :around ((decoder metainfo-decoder) type stream)
  (with-accessors ((map object-map-of)) stream
    (let ((start  (flexi-stream-position stream))
          (value  (call-next-method))
          (finish (flexi-stream-position stream)))
      (setf (gethash value map) (cons start finish))
      value)))

(defun metainfo-decode (input-object)
  (bencode-decode input-object 'metainfo-decoder))

(defun metainfo-decode-file (filename)
  (bencode-decode-file filename 'metainfo-decoder))
