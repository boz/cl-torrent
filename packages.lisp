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
           :metainfo-decode-file
           :bencmap->list

           ;; metainfo
           :info
           :info-hash
           :announce
           :announce-list
           :creation-date
           :comment
           :created-by

           ;; info-hash & files
           :piece-length
           :pieces
           :private
           :name
           :byte-length
           :md5sum
           :files
           :path))

(defpackage :cl-torrent.test
  (:use :cl :cl-torrent :fiveam)
  (:export :run-cl-torrent-tests
           :get-test-torrents))