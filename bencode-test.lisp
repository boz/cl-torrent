(in-package :cl-torrent)

(eval-when (:compile-toplevel)
  (defparameter *test-directory* 
    (make-pathname :directory (pathname-directory *compile-file-pathname*)
                   :name "tests")))

(defun make-testfile-name (file-name)
  (concatenate 'string (namestring *test-directory*) "/" file-name))

(defmacro decode-equal (expect input
                        &optional (decode-func 'bencode-decode)
                                  (oxform      'identity)
                                  (ixform      'identity))
  `(assert-equal ,expect (,oxform (,decode-func (,ixform ,input)))))

(defmacro fdecode-equal (expect file &optional (oxform 'identity))
  `(decode-equal ,expect ,file bencode-decode-file ,oxform make-testfile-name))

(defmacro fsdecode-equal (expect file)
  `(fdecode-equal ,expect ,file octets->string))

(defmacro sdecode-equal (expect input)
  `(decode-equal ,expect ,input bencode-decode octets->string))

(define-test octets->string
  (assert-equal "foo" (octets->string #(102 111 111))))

(define-test string->octets
  (assert-equalp #(102 111 111) (string->octets "foo")))

(define-test decode-string
  (sdecode-equal "foo" "3:foo")
  (sdecode-equal "" "0:")
  (fsdecode-equal "foo" "string-1.torrent"))

(define-test decode-integer
  (decode-equal 20 "i20e")
  (fdecode-equal 10 "integer-1.torrent"))

(define-test metainfo-decode-string
  (multiple-value-bind (val map buf)
      (metainfo-decode "3:foo")
    (assert-equal "foo" (octets->string val))
    (assert-equal 5 (length buf))
    (assert-equal '(0 . 5) (gethash val map))))

(define-test decode-list
  (decode-equal  '(30 40) "li30ei40ee")
  (fdecode-equal '(10 20) "list-1.torrent"))

(define-test decode-dict
  (let ((dict (bencode-decode "d3:fooi20ee")))
   (multiple-value-bind (val exists)
       (gethash "foo" dict)
     (assert-equal t exists)
     (assert-equal 20 val))))

;; (define-test test-tstream
;;   (with-tstream (sio "foo")
;;     (assert-equal #\f (stream-read-char sio))
;; ;;     (read-char sio)
;;     (read-char sio)
;;     (let ((buf (output-buffer-of sio)))
;;       (assert-true buf)
;;       (print buf)
;;       (print (get-output-stream-sequence buf))
;;       (assert-equal 3 (output-stream-sequence-length buf)))))