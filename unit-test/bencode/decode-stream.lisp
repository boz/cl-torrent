(in-package :cl-torrent-test)

(deftestsuite test-decode-stream (cl-torrent-test-suite) ())

(defmacro with-ds ((name buffer) &body body)
  `(let ((,name (make-decode-stream ,buffer)))
     ,@body
     (ensure-same (buffer ,name) ,buffer :test #'equalp)))

(addtest (test-decode-stream)
  test-make-decode-stream
  (with-ds (ds #(65 65))
    (ensure ds)
    (ensure-same (offset ds) 0)))

(addtest (test-decode-stream)
  test-peek-character
  
  (with-ds (ds #(65))
    
    (ensure-same (peek-character ds) #\A)
    (ensure-same (offset ds) 0)

    ;; only peek one character ahead.
    (ensure-same (peek-character ds) #\A)
    (ensure-same (offset ds) 0))

  ;; test EOF behavior
  (with-ds (ds #())
    (ensure-null (peek-character ds))
    (ensure-same (peek-character ds t nil :eof) :eof)
    (ensure-error (peek-character ds t t))
    (ensure-same  (offset ds) 0)))

(addtest (test-decode-stream)
  test-read-character
  
  (with-ds (ds #( 65 66))
    (ensure-same (read-character ds) #\A)
    (ensure-same (offset ds) 1)

    (ensure-same (read-character ds) #\B)
    (ensure-same (offset ds) 2)

    ;; EOF behavior
    (ensure-null (read-character ds))
    (ensure-same (read-character ds t nil :eof) :eof)
    (ensure-error (read-character ds t t))
    (ensure-same (offset ds) 2)))

(addtest (test-decode-stream)
  test-read-buffer
  (with-ds (ds #(65 66))
    (let ((x (read-buffer ds 1)))
      (ensure-same x #(65) :test #'equalp)
      (ensure-same (offset ds) 1))
    
    (ensure-same (read-buffer ds 1) #(66) :test #'equalp)
    (ensure-same (offset ds) 2)

    (ensure-null (read-buffer ds 1))
    (ensure-same (read-buffer ds 1 nil :eof) :eof)
    (ensure-error (read-buffer ds 1 t))
    (ensure-same (offset ds) 2)))