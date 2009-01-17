(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :cl-torrent-test)

(deftestsuite test-decode (cl-torrent-bencode-test-suite) ())

(defmacro with-decoded-vals ((obj val) input &body body)
  `(let* ((,obj (bencode-decode ,input))
         (,val (bencode-object-value ,obj)))
     ,@body))

(addtest (test-decode)
  test-string-decode
  (with-decoded-vals (x val) "3:AAA"
    (ensure-same val #( 65 65 65) :test #'equalp)))

(addtest (test-decode)
  test-integer-decode
  (with-decoded-vals (x val) "i10e"
    (ensure-same val 10)))

(addtest (test-decode)
  test-list-decode
  (with-decoded-vals (x val) "li10e3:AAAe"
    (ensure (listp val))
    (destructuring-bind (num str) val
      (ensure (typep num 'bencode-integer))
      (ensure-same 10 (bencode-object-value num))
      (ensure (typep str 'bencode-string))
      (ensure-same #(65 65 65) (bencode-object-value str) :test #'equalp)))
  (with-decoded-vals (x val) "li10ee"
    (ensure (listp val))
    (ensure-same 1 (length val))
    (ensure-same 10 (bencode-object-value (car val)))))

(addtest (test-decode)
  test-dict-decode
  (with-decoded-vals (x val) "d3:AAAi10ee"
    (ensure (typep x 'bencode-dictionary))
    (let ((value (bencode-dictionary-get x "AAA")))
      (ensure (typep value 'bencode-integer))
      (ensure-same 10 (bencode-object-value value)))))

(addtest (test-decode)
  test-metainfo-smoketest
  (dolist (i (get-test-torrents))
    (ensure (bencode-decode-file i))))