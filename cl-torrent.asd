(in-package :cl-user)

(defpackage :cl-torrent.asd
  (:use :cl :asdf))

(in-package :cl-torrent.asd)

(defsystem cl-torrent
    :name "cl-torrent"
    :components ((:file "packages")
                 (:file "bencode-utils"
                        :depends-on ("packages"))
                 (:file "bencode"
                        :depends-on ("bencode-utils" "packages"))
                 (:file "bencode-test"
                        :depends-on ("bencode-utils" "bencode" "packages")))
    :depends-on (:lisp-unit))

