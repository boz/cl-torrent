(in-package :cl-user)

(defpackage :cl-torrent
  (:use :cl :flexi-streams)
  (:export :bencode-decode
           :bencode-decode-file
           :string->octets
           :octets->string))

(defpackage :cl-torrent.test
  (:use :cl :cl-torrent :5am)
  (:export :run-cl-torrent-tests))