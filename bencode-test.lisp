(in-package :cl-torrent.test)

(defparameter *test-directory* nil)

(defun make-testfile-name (file-name)
  (concatenate 'string (namestring *test-directory*) "/" file-name))

(defun get-test-torrents ()
  (let ((spec (make-pathname :directory (make-testfile-name "torrents")
                             :name :wild
                             :type "torrent")))
    (directory spec)))

(defmacro decode-equal (expect input
                        &optional (decode-func 'bencode-decode)
                                  (oxform      'identity)
                                  (ixform      'identity))
  `(5am:is (equal ,expect (,oxform (,decode-func (,ixform ,input))))))

(defmacro fdecode-equal (expect file &optional (oxform 'identity))
  `(decode-equal ,expect ,file bencode-decode-file ,oxform make-testfile-name))

(defmacro fsdecode-equal (expect file)
  `(fdecode-equal ,expect ,file octets->string))

(defmacro sdecode-equal (expect input)
  `(decode-equal ,expect ,input bencode-decode octets->string))

(5am:test octets->string
  (5am:is (string= "foo" (octets->string #(102 111 111)))))

(5am:test string->octets
  (5am:is (equalp #(102 111 111) (string->octets "foo"))))

(5am:test decode-string
  (sdecode-equal "foo" "3:foo")
  (sdecode-equal "" "0:")
  (fsdecode-equal "foo" "string-1.torrent"))

(5am:test decode-integer
  (decode-equal 20 "i20e")
  (fdecode-equal 10 "integer-1.torrent"))

(5am:test decode-list
  (decode-equal  '(30 40) "li30ei40ee")
  (5am:is (equalp '(#(65) #(65)) (bencode-decode "l1:A1:Ae")))
  (fdecode-equal '(10 20) "list-1.torrent"))

(5am:test decode-dict
  (let ((dict (bencode-decode "d3:fooi20ee")))
   (multiple-value-bind (val exists)
       (gethash "foo" dict)
     (5am:is-true exists)
     (5am:is (= 20 val)))))

(5am:test cached-decode-string
  (multiple-value-bind (val map buf)
      (bencode-decode "3:foo" t)
    (5am:is (string= "foo" (octets->string val)))
    (5am:is (= 5 (length buf)))
    (5am:is (equalp '(0 . 5) (gethash val map)))))

(5am:test test-torrents
  (dolist (i (get-test-torrents))
    (5am:is-true  (bencode-decode-file i))
    (5am:is-true  (bencode-decode-file i t))))

(5am:test metainfo-test-torrents
  (dolist (i (get-test-torrents))
    (5am:is-true (metainfo-decode-file i))))

(defun run-cl-torrent-tests ()
  (5am:run!))

