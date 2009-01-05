(in-package :cl-user)

(defpackage :cl-torrent.asd
  (:use :cl :asdf))

(in-package :cl-torrent.asd)

(defsystem cl-torrent
    :name "cl-torrent"
    :components
    ((:module utils
              :components
              ((:file "packages")
               (:file "utils"
                      :depends-on ("packages"))
               (:file "stream"
                      :depends-on ("packages"))
               (:file "filesystem"
                      :depends-on ("packages"))))
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
              :depends-on (utils bencode))

     (:module net
              :components
              ((:file "packages")
               (:file "utils"
                      :depends-on ("packages")))
              :depends-on (utils)))
  
    :depends-on (:flexi-streams :ironclad :drakma :cl-fad :sb-posix)

    :in-order-to ((test-op (test-op cl-torrent-test))))
