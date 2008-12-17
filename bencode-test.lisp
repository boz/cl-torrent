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

(defmacro fassert-equal (expect file &optional (xform 'identity))
  `(assert-equal ,expect (,xform (decode-test-file ,file))))

(defmacro fassert-equal-string (expect file)
  `(fassert-equal ,expect ,file buf->string))

(define-test buf->string
  (assert-equal "foo" (buf->string #(102 111 111))))

(define-test decode-string
  (fassert-equal-string "foo" "string-1.torrent"))

(define-test decode-integer
  (fassert-equal 10 "integer-1.torrent"))

(define-test decode-list
  (fassert-equal (list 10 20)
                 "list-1.torrent"))
