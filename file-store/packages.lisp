(in-package :cl-user)
(defpackage :cl-torrent.file-store
  (:use :cl :iterate)
  (:export :make-range
           :copy-range
           :range-start
           :range-end
           :range-length
           :make-range-set
           :range-set-fit
           :range-set-ranges
           :range-set-length
           :range-set-start
           :range-set-end
           :range-set-lengt))