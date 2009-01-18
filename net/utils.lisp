(in-package :cl-torrent.net)

(defun http-get-binary (uri &optional parameters)
  (multiple-value-bind
        (body status headers uri stream must-close reason-phrase)
      (drakma:http-request uri
                           :method       :get
                           :parameters   parameters
                           :force-binary t
                           :want-stream  t)
    (declare (ignore headers uri stream must-close reason-phrase))
    (with-open-stream (body body)
      (assert (= 200 status))
      (copy-stream-to-buffer body))))
