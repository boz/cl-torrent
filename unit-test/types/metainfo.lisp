(in-package :cl-torrent-test)

(deftestsuite test-types-metainfo (cl-torrent-test-suite) ())

(addtest (test-types-metainfo) test-metainfo
  (dolist (i (get-test-torrents))
    (ensure (metainfo-decode-file i))))
