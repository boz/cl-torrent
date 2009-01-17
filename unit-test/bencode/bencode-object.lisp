(in-package :cl-torrent-test)

(deftestsuite test-bencode-object (cl-torrent-bencode-test-suite) ())

(addtest (test-bencode-object)
  test-smoke
  (make-instance 'bencode-string)
  (make-instance 'bencode-integer)
  (make-instance 'bencode-dictionary)
  (make-instance 'bencode-list))

(addtest (test-bencode-object)
  test-bencode-dictionary
  (let ((x (make-instance 'bencode-dictionary)))
    (bencode-dictionary-set x "foo" "bar")
    (ensure-same "bar" (bencode-dictionary-get x "foo"))))

(addtest (test-bencode-object)
  test-bencode-object=
  (let ((x (make-bencode-string "foo"))
        (y (make-bencode-string "bar")))
    (ensure-null (bencode-object= x y)))
  (let ((x (make-bencode-string "foo"))
        (y (make-bencode-string "foo")))
    (ensure (bencode-object= x y))))
