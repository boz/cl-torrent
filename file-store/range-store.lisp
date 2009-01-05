(in-package :cl-torrent.file-store)

(defclass range-store ()
  ((size :accessor size :initarg :size)
   (filled-ranges :accessor filled-ranges :initform nil)))

