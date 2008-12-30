(in-package :cl-user)

(defpackage :cl-torrent.asd
  (:use :cl :asdf))

(in-package :cl-torrent.asd)

(defsystem cl-torrent
  :name "cl-torrent"
  :components
  ((:file "packages")
   (:file "utils"
          :depends-on ("packages"))
   (:file "decode-ctx"
          :depends-on ("packages"))
   (:file "bencode-utils"
          :depends-on ("packages"))
   (:file "bencode"
          :depends-on ("bencode-utils"))
   (:file "metainfo-lib"
          :depends-on ("utils" "decode-ctx" "bencode"))
   (:file "metainfo"
          :depends-on ("metainfo-lib")))
  :depends-on (:flexi-streams :ironclad :fiveam))

(defsystem cl-torrent.test
  :name "cl-torrent.test"
  :components ((:file "packages")
               (:file "bencode-test"))
  :depends-on (:cl-torrent :fiveam))

(defmethod asdf:perform :before ((op asdf:load-op)
                                 (system (eql (find-system :cl-torrent.test))))
  (setf (symbol-value (intern "*TEST-DIRECTORY*" "CL-TORRENT.TEST"))
        (asdf:system-relative-pathname system "tests")))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (find-system :cl-torrent.test))))
  (asdf:oos 'asdf:load-op "cl-torrent.test")
  (funcall (intern (string :run-cl-torrent-tests) (string :cl-torrent.test))))
