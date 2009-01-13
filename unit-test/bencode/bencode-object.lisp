
(in-package :cl-torrent-test)

(deftestsuite test-bencode-object (cl-torrent-bencode-test-suite) ())

(addtest (test-bencode-object)
  (make-instance 'bencode-string)
  (make-instance 'bencode-integer)
  (make-instance 'bencode-dictionary)
  (make-instance 'bencode-list))
