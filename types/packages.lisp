(in-package :cl-user)

(defpackage :cl-torrent.types
  (:use :cl :cl-torrent.utils :cl-torrent.bencode)
  (:import-from :ironclad :digest-sequence :byte-array-to-hex-string)
  (:export

   ;; metainfo
   :metainfo-decode
   :metainfo-decode-file

   ;; metainfo slots
   :info
   :info-hash
   :announce
   :announce-list
   :creation-date
   :comment
   :created-by

   ;; info-hash & files slots
   :piece-length
   :pieces
   :private
   :name
   :byte-length
   :md5sum
   :files
   :path))