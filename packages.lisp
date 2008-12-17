(in-package :cl-user)

(defpackage :cl-torrent.bencode
  (:use :cl :lisp-unit)
  (:export
   :decode
   :encode))

(defpackage :cl-torrent.bencode.test
  (:use :cl :lisp-unit :cl-torrent.bencode))