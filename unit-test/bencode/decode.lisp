
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
