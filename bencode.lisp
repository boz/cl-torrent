(in-package :cl-torrent)

(defclass bencode-input-stream (flexi-input-stream) ())
(defclass bencode-decoder () ())

(defgeneric make-decoding-stream (decoder input-object))
(defgeneric next-object-type (decoder stream))
(defgeneric decode-from-stream (decoder stream))
(defgeneric decode-object (decoder type stream))

(defmethod make-decoding-stream ((decoder bencode-decoder)
                                 (stream stream))
  (make-instance 'bencode-input-stream
                 :stream stream
                 :element-type '(unsigned-byte 8)
                 :flexi-stream-external-format *bencode-external-format*))

(defmethod make-decoding-stream  ((decoder bencode-decoder)
                                  (string string))
  (make-decoding-stream decoder (string->octets string)))

(defmethod make-decoding-stream ((decoder bencode-decoder)
                                 (seq vector list))
  (make-decoding-stream decoder (make-in-memory-input-stream seq)))

(defmethod next-object-type ((decoder bencode-decoder) stream)
  (let ((c (peek-char nil stream t)))
    (cond
      ((char= c #\d)    'dictionary)
      ((char= c #\l)    'list)
      ((char= c #\i)    'integer)
      ((digit-char-p c) 'string))))

(defmethod decode-from-stream ((decoder bencode-decoder) stream)
  (let ((type (next-object-type decoder stream)))
    (when type
      (decode-object decoder type stream))))

(defmacro assert-next-char (char stream)
  `(assert (char= ,char (code-char (read-byte ,stream)))))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'string))
                          stream)
  (let ((len (read-decimal stream)))
    (assert-next-char #\: stream)
    (let* ((buf  (make-array len))
           (rlen (read-sequence buf stream)))
      (assert (= rlen len))
      buf)))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'integer))
                          stream)
  (assert-next-char #\i stream)
  (let ((val (read-decimal stream)))
    (assert-next-char #\e stream)
    val))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'dictionary))
                          stream)
  (assert-next-char #\d stream)
  (do ((type (next-object-type decoder stream)
             (next-object-type decoder stream))
       (dict (make-hash-table :test #'equal)))
      ((and (not type) (char= #\e (read-char stream)))
       dict)
    (assert (eq type 'string)) 
    (let* ((key  (decode-object decoder type stream))
           (key  (octets->string key))
           (type (next-object-type decoder stream)))
      (assert type)
      (setf (gethash key dict)
            (decode-object decoder type stream)))))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'list))
                          stream)
  (assert-next-char #\l stream)
  (do ((type (next-object-type decoder stream)
             (next-object-type decoder stream))
       (lst nil))
      ((and (not type) (char= #\e (read-char stream)))
       (nreverse lst))
    (assert type)
    (setf lst
          (cons (decode-object decoder type stream) lst))))

(defun decode (input-object &optional (type 'bencode-decoder))
  (let* ((decoder (make-instance type))
         (stream  (make-decoding-stream decoder input-object)))
    (decode-from-stream decoder stream)))
