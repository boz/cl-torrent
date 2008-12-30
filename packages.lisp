(in-package :cl-user)

(defpackage :cl-torrent
  (:use :cl :flexi-streams)
  (:import-from :ironclad :digest-sequence
                          :byte-array-to-hex-string)
  (:export :bencode-decode
           :bencode-decode-file
           :string->octets
           :octets->string
           :metainfo-decode
           :metainfo-decode-file))

(defpackage :cl-torrent.test
  (:use :cl :cl-torrent :5am)
  (:export :run-cl-torrent-tests
           :get-test-torrents))