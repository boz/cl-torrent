(in-package :cl-user)

(defpackage :cl-torrent.bencode
  (:use :cl :cl-torrent.utils :flexi-streams)
  (:export

   ;; utils
   :string->octets
   :octets->string

   ;; bencode
   :bencode-decode
   :bencode-decode-file

   ;; bencmap
   :bencmap-decode
   :bencmap-decode-file
   :decode-bencmap
   :decode-bencmap-value
   :bencmap->list))
