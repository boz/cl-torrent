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
   :bencode-string-value
   :bencode-integer
   :bencode-dictionary
   :bencode-list
   :bencode-object=
   :bencode-dictionary-get
   :bencode-dictionary-set
   :make-bencode-string
   :make-bencode-integer
   :make-bencode-dictionary
   :make-bencode-list
   

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
   :define-bencmap-class
   :bencmap-decode
   :bencmap-decode-file
   :decode-bencmap
   :decode-bencmap-value
   :bencmap->list
   :with-bencode-value))
