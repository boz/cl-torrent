(in-package :cl-user)

(defpackage :cl-torrent.asd
  (:use :cl :asdf))

(in-package :cl-torrent.asd)

(defsystem cl-torrent.bencode
    :name "cl-torrent.bencode"
    :components ((:file "packages")
                 (:file "bencode"      :depends-on ("packages"))
                 (:file "bencode-test" :depends-on ("bencode")))
    :depends-on (:lisp-unit))

