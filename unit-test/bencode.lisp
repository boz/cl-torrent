(in-package :cl-torrent-test)

(deftestsuite bencode-test-suite () ())

(defmacro decode-equal (expect input
                        &optional (decode-func 'bencode-decode)
                        (oxform      'identity)
                        (ixform      'identity))
  `(ensure-same ,expect
               (,oxform (,decode-func (,ixform ,input)))
               :test #'equal))

(defmacro fdecode-equal (expect file &optional (oxform 'identity))
  `(decode-equal ,expect ,file bencode-decode-file ,oxform make-testfile-name))

(defmacro fsdecode-equal (expect file)
  `(fdecode-equal ,expect ,file octets->string))

(defmacro sdecode-equal (expect input)
  `(decode-equal ,expect ,input bencode-decode octets->string))

(deftestsuite test-bencode-utils (bencode-test-suite) ())

(addtest (test-bencode-utils) test-octets->string
         (ensure-same "foo" (octets->string #(102 111 111)) :test #'string=))

(addtest (test-bencode-utils) test-string->octets
         (ensure-same #(102 111 111) (string->octets "foo") :test #'equalp))

(deftestsuite test-bencode-decode (bencode-test-suite) ())
(addtest (test-bencode-decode) test-decode-string
  (sdecode-equal "foo" "3:foo")
  (sdecode-equal "" "0:")
  (fsdecode-equal "foo" "string-1.torrent"))

(addtest (test-bencode-decode) test-decode-integer
  (decode-equal 20 "i20e")
  (fdecode-equal 10 "integer-1.torrent"))

(addtest (test-bencode-decode) test-decode-list
  (decode-equal  '(30 40) "li30ei40ee")
  (ensure-same '(#(65) #(65)) (bencode-decode "l1:A1:Ae") :test #'equalp)
  (fdecode-equal '(10 20) "list-1.torrent"))

(addtest (test-bencode-decode) test-decode-dict
  (let ((dict (bencode-decode "d3:fooi20ee")))
    (multiple-value-bind (val exists)
        (gethash "foo" dict)
      (ensure exists :report "foo not in dict")
      (ensure-same 20 val))))

(addtest (test-bencode-decode) test-cached-decode-string
  (multiple-value-bind (val map buf)
      (bencode-decode "3:foo" t)
    (ensure-same "foo" (octets->string val) :test #'string=)
    (ensure-same 5 (length buf))
    (ensure-same '(0 . 5) (gethash val map) :test #'equalp)))

(addtest (test-bencode-decode) test-torrents
  (dolist (i (get-test-torrents))
    (ensure (bencode-decode-file i))
    (ensure (bencode-decode-file i t))))

(deftestsuite test-types-metainfo (bencode-test-suite) ())
(addtest (test-types-metainfo) test-metainfo
  (dolist (i (get-test-torrents))
    (ensure (metainfo-decode-file i))))
