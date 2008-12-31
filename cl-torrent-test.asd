(in-package :cl-user)

(defpackage :cl-torrent-test.asd
  (:use :cl :asdf))

(in-package :cl-torrent-test.asd)

(defsystem cl-torrent-test
    :name "cl-torrent-test"
    :components
    ((:module unit-test
              :components
              ((:file "packages")
               (:file "tests"
                      :depends-on ("packages"))
               (:file "bencode"
                      :depends-on ("packages" "tests")))))
    :depends-on (:cl-torrent :fiveam))

(defmethod asdf:perform :before
    ((op asdf:load-op) (system (eql (find-system :cl-torrent-test))))
  (setf (symbol-value (intern "*TEST-DIRECTORY*" "CL-TORRENT-TEST"))
        (asdf:system-relative-pathname system "test-data")))

(defmethod asdf:perform
    ((op asdf:test-op) (system (eql (find-system :cl-torrent-test))))
  (asdf:oos 'asdf:load-op "cl-torrent-test")
  (funcall (intern "RUN-TESTS" "CL-TORRENT-TEST")))

