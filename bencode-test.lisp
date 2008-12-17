
(in-packge :cl-torrent.bencode.test)

(eval-when (:compile-toplevel)
  (defparameter *test-directory* 
    (make-pathname :directory (pathname-directory *compile-file-pathname*)
                   :name "tests")))

(defmacro with-test-file (stream-name file-name &body body)
  `(with-open-file (,stream-name (make-pathname :directory *test-directory*
                                                :name ,file-name)
                                 :element-type '(unsigned-byte 8))
     ,@body))

(defun decode-test-file (name)
  (with-test-file (stream name)
    (bencode:decode stream)))
