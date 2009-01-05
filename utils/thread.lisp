(in-package :cl-torrent.utils)

(defun make-mutex (&key name)
  (sb-thread:make-mutex :name name))
