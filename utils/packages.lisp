(in-package :cl-user)

(defpackage :cl-torrent.utils
  (:use :cl :cl-fad)
  (:import-from :flexi-streams
                :make-in-memory-output-stream
                :get-output-stream-sequence)
  (:import-from :cl-fad :pathname-as-directory)
  (:export
   :with-gensyms
   :symbol->keyword

   :generate-random-bytes
   
   ;; stream
   :copy-stream-to-buffer
   :*stream-buffer-size*

   ;; filesystem
   :make-directory
   :user-homedir

   :make-mutex))
