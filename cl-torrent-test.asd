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

  :depends-on (:cl-torrent :lift)

  :perform
  (load-op :before (op c)
           (let ((sym (intern "*TEST-DIRECTORY*" "CL-TORRENT-TEST")))
             (setf (symbol-value sym)
                   (system-relative-pathname c "test-data")))))

;;   :perform
;;   (test-op :before (op c)
;;            (break "WTF")
;;            (funcall (intern "RUN-TESTS!" "CL-TORRENT-TEST")))

(defmethod asdf:perform :after
    ((op test-op) (c (eql (find-system :cl-torrent-test))))
  (funcall (intern "RUN-TESTS!" "CL-TORRENT-TEST")))