(in-package :cl-torrent)

(eval-when (:compile-toplevel)
  (defparameter *test-directory* 
    (make-pathname :directory (pathname-directory *compile-file-pathname*)
                   :name "tests")))

(defun make-testfile-name (file-name)
  (concatenate 'string
               (namestring *test-directory*) "/" file-name))

(defmacro with-test-file ((stream-name file-name) &body body)
  `(with-open-file (,stream-name (make-testfile-name ,file-name)
                                 :element-type '(unsigned-byte 8))
     (progn ,@body)))

(defun decode-test-file (name)
  (with-test-file (stream name)
    (decode stream)))

(defmacro fassert-equal (expect file &optional (xform 'identity))
   `(assert-equal ,expect (,xform (decode-test-file ,file))))

(defmacro fassert-equal-string (expect file)
  `(fassert-equal ,expect ,file octets->string))

(define-test octets->string
  (assert-equal "foo" (octets->string #(102 111 111))))

(define-test string->octets
  (assert-equalp #(102 111 111) (string->octets "foo")))

(define-test decode-string
  (assert-equal "foo" (octets->string (decode "3:foo"))))

(define-test decode-integer
  (assert-equal 20 (decode "i20e")))

(define-test decode-list
  (assert-equal '(30 40) (decode "li30ei40ee")))

(define-test decode-dict
  (let ((dict (decode "d3:fooi20ee")))
   (multiple-value-bind (val exists)
       (gethash "foo" dict)
     (assert-equal t exists)
     (assert-equal 20 val))))
