(in-package :cl-torrent)

(eval-when (:compile-toplevel)
  (defparameter *test-directory* 
    (make-pathname :directory (pathname-directory *compile-file-pathname*)
                   :name "tests")))

(defun make-testfile-name (file-name)
  (concatenate 'string
               (namestring *test-directory*) "/" file-name))

(defmacro with-test-file (stream-name file-name &body body)
  `(with-open-file (,stream-name (make-testfile-name ,file-name)
                                 :element-type '(unsigned-byte 8))
     (progn ,@body)))

(defun decode-test-file (name)
  (with-test-file stream name
    (decode stream)))

(defmacro assert-string (expect val)
  `(assert-equal ,expect (buf->string ,val)))

(defmacro fassert-string (expect fname)
  `(assert-string ,expect (decode-test-file ,fname)))

(define-test test-decode-string
  (fassert-string "foo" "string-1.torrent"))

