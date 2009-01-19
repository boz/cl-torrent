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
             (:file "decode-stream"
                    :depends-on ("packages" "utils"))
             (:file "bencode-object"
                    :depends-on ("packages" "utils"))
             (:file "decode"
                    :depends-on 
                    ("packages" "utils" "decode-stream" "bencode-object"))
             (:file "bencmap"
                    :depends-on
                    ("packages" "decode")))
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
            :depends-on (utils))

   (:module file-store
            :components
            ((:file "packages")
             (:file "ranges"
                    :depends-on ("packages")))))

  :depends-on (:flexi-streams :ironclad :drakma :cl-fad :sb-posix)

  :in-order-to ((test-op (test-op cl-torrent-test))))

