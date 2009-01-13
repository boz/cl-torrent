(in-package :cl-user)

(defpackage :cl-torrent.bencode
  (:use :cl :cl-torrent.utils :flexi-streams)
  (:export
   
   ;; utils
   :string->octets
   :octets->string
   :ensure-octets

   ;; bencode-object
   :bencode-object
   :bencode-object-bytes
   :bencode-object-value
   :bencode-string
   :bencode-integer
   :bencode-dictionary
   :bencode-list
   :bencode-object=
   :bencode-dictionary-get
   :bencode-dictionary-set

   ;; decode stream
   :decode-stream-offset
   :decode-stream-buffer
   :peek-character
   :read-character
   :read-buffer
   :make-decode-stream

   ;; decode
   :bencode-decode
   :bencode-decode-file

   ;; bencmap
;;    :define-bencmap-class
;;    :bencmap-decode
;;    :bencmap-decode-file
;;    :decode-bencmap
;;    :decode-bencmap-value
;;    :bencmap->list

   ))
