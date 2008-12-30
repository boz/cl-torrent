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
