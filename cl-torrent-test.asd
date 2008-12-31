(in-package :cl-user)

(defpackage :cl-torrent-test.asd
  (:use :cl :asdf))

(in-package :cl-torrent-test.asd)

(defsystem cl-torrent.test
  :name "cl-torrent.test"
  :components
  ((:module unit-test
            :components
            ((:file "packages")
             (:file "bencode" :depends-on ("packages")))))
  :depends-on (:cl-torrent :fiveam))

(defmethod asdf:perform :before ((op asdf:load-op)
                                 (system (eql (find-system :cl-torrent.test))))
  (setf (symbol-value (intern "*TEST-DIRECTORY*" "CL-TORRENT.TEST"))
        (asdf:component-relative-pathname system "tests")))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (find-system :cl-torrent.test))))
  (asdf:oos 'asdf:load-op "cl-torrent.test")
  (funcall (intern (string :run-cl-torrent-tests) (string :cl-torrent.test))))