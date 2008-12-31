(in-package :cl-user)

(defpackage :cl-torrent.bencode
  (:use :cl :cl-torrent.utils :flexi-streams)
  (:export

   ;; utils
   :string->octets
   :octets->string

   ;; decode
   :bencode-decode
   :bencode-decode-file

   ;; decode-ctx
   :bencmap-decode-ctx
   :new-decode-ctx
   :decode-ctx-contains
   :decode-ctx-get-range
   :decode-ctx-get-bytes
   :decode-ctx-get
   
   ;; bencmap
   :define-bencmap-class
   :bencmap-decode
   :bencmap-decode-file
   :decode-bencmap
   :decode-bencmap-value
   :bencmap->list))
