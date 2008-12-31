(in-package :cl-user)

(defpackage :cl-torrent.asd
  (:use :cl :asdf))

(in-package :cl-torrent.asd)

(defsystem cl-torrent
    :name "cl-torrent"
    :components
    ((:module utils
              :serial t
              :components
              ((:file "packages")
               (:file "utils")))

     (:module bencode
              :components
              ((:file "packages")
               (:file "utils"
                      :depends-on ("packages"))
               (:file "decode"
                      :depends-on ("packages" "utils"))
               (:file "decode-ctx"
                      :depends-on ("packages" "utils" "decode"))
               (:file "bencmap"
                      :depends-on ("packages" "utils" "decode" "decode-ctx")))
              :depends-on (utils))

     (:module types
              :components
              ((:file "packages")
               (:file "metainfo"
                      :depends-on ("packages")))
              :depends-on (utils bencode)))

    :depends-on (:flexi-streams :ironclad)

    :in-order-to ((test-op (test-op cl-torrent-test))))
