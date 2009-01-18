(in-package :cl-user)

(defpackage :cl-torrent.types
  (:use :cl :cl-torrent.utils :cl-torrent.bencode)
  (:import-from :ironclad :digest-sequence :byte-array-to-hex-string)
  (:export

   ;; metainfo
   :metainfo-decode
   :metainfo-decode-file

   ;; metainfo slots
   :metainfo-info
   :metainfo-info-hash
   :metainfo-announce
   :metainfo-announce-list
   :metainfo-creation-date
   :metainfo-comment
   :metainfo-created-by

   ;; info-hash & files slots
   :info-piece-length
   :info-byte-length
   :info-pieces
   :info-name
   :info-private
   :info-md5sum
   :info-files
   :info-file-byte-length
   :info-file-md5sum
   :info-file-path))