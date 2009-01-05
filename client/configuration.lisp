(in-package :cl-torrent.client)

(defvar *default-cache-dir*)
(defvar *default-download-dir*)
(defvar *default-client-id*)
(defvar *default-listen-ports* '(6881 6882))

(eval-when (:load-toplevel :execute)
  (setf *default-cache-dir*
        (pathname-as-directory
         (make-pathname :directory (pathname-directory (user-homedir))
                        :name      ".cl-torrent.cache")))
  
  (setf *default-cache-dir*
        (pathname-as-directory
         (make-pathname :directory (pathname-directory (user-homedir))
                        :name      "cl-torrent-downloads")))
  
  (setf *default-client-id*
       (byte-array-to-hex-string (make-random-bytes 10))))

(defclass configuration ()
  ((cache-dir
    :reader cache-dir :initarg :cache-dir
    :initform *default-cache-dir*)
   (download-dir
    :reader download-dir :initarg :download-dir
    :initform *default-download-dir*)
   (client-id
    :reader client-id :initarg :client-id
    :initform *default-client-id*)
   (listen-ports
    :reader port :initarg :port
    :initform *default-listen-ports*)))

