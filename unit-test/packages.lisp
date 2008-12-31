(in-package :cl-user)

(defpackage :cl-torrent-test
  (:use :cl :fiveam
        :cl-torrent.utils
        :cl-torrent.bencode
        :cl-torrent.types)

  (:export :*test-directory*
           :make-testfile-name
           :get-test-torrents
           :run-tests))
