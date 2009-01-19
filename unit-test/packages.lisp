(in-package :cl-user)

(defpackage :cl-torrent-test
  (:use :cl :lift
        :cl-torrent.utils
        :cl-torrent.bencode
        :cl-torrent.types
        :cl-torrent.file-store)

  (:export :*test-directory*
           :make-testfile-name
           :get-test-torrents
           :run-tests!))
