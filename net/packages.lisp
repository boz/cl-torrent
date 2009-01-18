(in-package :cl-user)

(defpackage :cl-torrent.net
  (:use :cl :cl-torrent.utils)
  (:export :http-get-binary))