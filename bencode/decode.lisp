(in-package :cl-torrent.bencode)

(defclass bencode-input-stream (flexi-input-stream) ()
  (:documentation
   "An INPUT-STREAM with an :ELEMENT-TYPE of '(UNSIGNED-BYTE 8) that
supports PEEK-CHAR and READ-CHAR."))

(defclass bencode-decoder () ())

(defgeneric make-decoding-stream (decoder input-object)
  (:documentation "Create an input stream necessary for decoding."))

(defgeneric next-object-type (decoder stream)
  (:documentation "Determine the next object type in the given stream."))

(defgeneric decode-from-stream (decoder stream)
  (:documentation "Decode an object from the given stream."))

(defgeneric decode-object (decoder type stream)
  (:documentation "Decode the object of type TYPE from the given stream"))

(defmethod make-decoding-stream ((decoder bencode-decoder) stream)
  (ensure-flexi-stream stream))

(defmethod next-object-type ((decoder bencode-decoder) stream)
  "Use PEEK-CHAR to determine the next object type.  If EOF is reached or the
next character does not signal an object type, return NIL."
  (let ((c (peek-char nil stream t nil)))
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
  "Read the next character from the stream, and assert it's value."
  `(assert (char= ,char (code-char (read-byte ,stream)))))

(defmethod decode-object ((decoder bencode-decoder) (type (eql 'string)) stream)
  "Decode a bencoded string."
  (let ((len (read-decimal stream)))
    (assert-next-char #\: stream)
    (let ((buf (make-array len :element-type 'octet)))
      ;; read-sequence doesn't update file position
      (dotimes (i len buf)
        (setf (aref buf i) (read-byte stream))))))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'integer))
                          stream)
  "Decode a bencoded integer."
  (assert-next-char #\i stream)
  (let ((val (read-decimal stream)))
    (assert-next-char #\e stream)
    val))

(defmethod decode-object ((decoder bencode-decoder)
                          (type (eql 'dictionary))
                          stream)
  "Decode a bencoded dictionary."
  (assert-next-char #\d stream) 
  (do ((type (next-object-type decoder stream)
             (next-object-type decoder stream))
       (dict (make-hash-table :test #'equal)))
      ((and (not type) (char= #\e (read-char stream)))
       dict)
    (assert (eq type 'string)) ;; keys must be a string
    (let* ((key  (decode-object decoder type stream))
           (key  (octets->string key))
           (type (next-object-type decoder stream)))
      (assert type) ;; a value is required
      (setf (gethash key dict)
            (decode-object decoder type stream)))))

(defmethod decode-object ((decoder bencode-decoder) (type (eql 'list)) stream)
  "Decode a bencoded list."
  (assert-next-char #\l stream)
  (do ((type (next-object-type decoder stream)
             (next-object-type decoder stream))
       (lst nil))
      ((and (not type) (char= #\e (read-char stream)))
       (nreverse lst))
    (assert type)
    (push (decode-object decoder type stream) lst)))

(defclass cached-bencode-input-stream (flexi-input-stream)
  ((object-map
    :initarg :object-map :accessor object-map-of)
   (output-stream
    :initarg :output-stream :accessor output-stream-of))
  (:documentation ""))

(defun make-cached-bencode-input-stream (stream)
  (let* ((out
          (make-flexi-stream (make-in-memory-output-stream)
                             :external-format *bencode-external-format*))
         (stream (make-echo-stream stream out)))
    (make-instance 'cached-bencode-input-stream
                   :stream stream
                   :element-type 'octet
                   :flexi-stream-external-format *bencode-external-format*
                   :object-map (make-hash-table :test 'eq)
                   :output-stream (flexi-stream-stream out))))

(defclass cached-bencode-decoder (bencode-decoder) ()
  (:documentation
   "Decode cached-bencode files."))

(defmethod decode-from-stream :around ((decoder cached-bencode-decoder) stream)
  (let* ((stream (make-cached-bencode-input-stream stream))
         (val    (call-next-method decoder stream))
         (map    (object-map-of stream))
         (buf    (get-output-stream-sequence (output-stream-of stream))))
    (values val map buf)))

(defmethod decode-object :around ((decoder cached-bencode-decoder) type stream)
  (with-accessors ((map object-map-of)) stream
    (let ((start  (flexi-stream-position stream))
          (value  (call-next-method))
          (finish (flexi-stream-position stream)))
      (setf (gethash value map) (cons start finish))
      value)))

(defun cached-bencode-decode (input-object)
  (bencode-decode input-object 'cached-bencode-decoder))

(defun cached-bencode-decode-file (filename)
  (bencode-decode-file filename 'cached-bencode-decoder))

(defun bencode-decode (input-object &optional (use-cache nil))
  "Decode the input-object which should be a SEQUENCE of bytes, a STRING,
or an INPUT-STREAM with an :ELEMENT-TYPE of '(UNSIGNED-BYTE 8)."
  (let* ((decoder (make-instance
                   (if use-cache 'cached-bencode-decoder 'bencode-decoder)))
         (stream  (make-decoding-stream decoder input-object)))
    (decode-from-stream decoder stream)))

(defun bencode-decode-file (pathname &optional (use-cache nil))
  "Decode the file named ``pathname''."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (bencode-decode stream use-cache)))