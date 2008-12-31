(in-package :cl-torrent-test)

(defparameter *test-directory* nil)

(defun make-testfile-name (file-name)
  (concatenate 'string (namestring *test-directory*) "/" file-name))

(defun get-test-torrents ()
  (let ((spec (make-pathname :directory (make-testfile-name "torrents")
                             :name :wild
                             :type "torrent")))
    (directory spec)))

(defun run-tests ()
  (5am:run!))
