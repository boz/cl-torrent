(in-package :cl-torrent-test)

(deftestsuite utils-test-suite (cl-torrent-test-suite) ())

(addtest (utils-test-suite)
  copy-stream-to-buffer
  (with-input-from-string (in "foo")
    (let ((buf (copy-stream-to-buffer in)))
      (ensure-same 3 (length buf))
      (ensure-same "foo" buf :test #'equalp)))
  (with-input-from-string (in "foo")
    (ensure-error (copy-stream-to-buffer in :max-len 2)))
  (with-input-from-string (in "foo")
    (let ((buf (copy-stream-to-buffer in :max-len 2 :signal-error? nil)))
      (ensure-same 2 (length buf))
      (ensure-same "fo" buf :test #'equalp))))

