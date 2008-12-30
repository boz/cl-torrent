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
                      :depends-on ("packages" "bencode-utils"))
               (:file "metainfo"
                      :depends-on ("packages" "bencode-utils" "bencode")))
  :depends-on (:flexi-streams :ironclad))

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
