(in-package :cl-torrent.file-store)

(defclass piece-spec ()
  ((bytes-size
    :reader byte-size :initarg byte-size)
   (bytes-downloaded
    :reader bytes-downloaded :initform 0)
   (ranges-left
    :reader ranges-downloaded :initform nil)
   (hash
    :reader hash :initarg hash)))

(defclass file-spec ()
  ((index  :accessor index  :initarg :index)
   (size   :accessor size   :initarg :size)
   (path   :accessor path   :initarg :path)
   (pieces :accessor pieces :initarg :pieces)))
