(in-package :cl-torrent.bencode)

(defun next-object-type (stream)
  "Use PEEK-CHARACTER to determine the next object type.  If EOF is reached or the
next character does not signal an object type, return NIL."
  (let ((c (peek-character stream t nil)))
    (cond
      ((char= c #\d)    'bencode-dictionary)
      ((char= c #\l)    'bencode-list)
      ((char= c #\i)    'bencode-integer)
      ((digit-char-p c) 'bencode-string))))

(defun make-next-object (stream)
  (let ((type (next-object-type stream)))
    (when type
      (make-instance type))))

(defmacro assert-next-char (char stream)
  "Read the next character from the stream, and assert it's value."
  `(assert (char= ,char (read-character ,stream))))

(defgeneric decode-bencode-object (obj stream))

(defmethod decode-bencode-object :around ((obj bencode-object) (stream decode-stream))
  (let ((start (decode-stream-offset stream))
        (val   (call-next-method))
        (end   (decode-stream-offset stream)))
    (declare (ignore val))
    (setf (bencode-object-bytes obj)
          (subseq (decode-stream-buffer stream) start end))
    obj))

(defmethod decode-bencode-object ((obj bencode-string) (stream decode-stream))
  "Decode a bencoded string"
  (let ((len (read-decimal stream)))
    (assert-next-char #\: stream)
    (setf (bencode-object-value obj) (read-buffer stream len t))))

(defmethod decode-bencode-object ((obj bencode-integer) (stream decode-stream))
  "Decode a bencoded integer"
  (assert-next-char #\i stream)
  (let ((val (read-decimal stream)))
    (assert-next-char #\e stream)
    (setf (bencode-object-value obj) val)))

(defmethod decode-bencode-object ((obj bencode-dictionary) (stream decode-stream))
  "Decode a bencoded dictionary."
  (assert-next-char #\d stream) 
  (do ((next-obj (make-next-object stream)
                 (make-next-object stream)))
      ((and (not next-obj) (char= #\e (read-character stream)))
       obj)
    (assert (typep next-obj 'bencode-string)) ;; keys must be a string
    (let* ((key  (decode-bencode-object next-obj stream))
           (next-object (make-next-object stream)))
      (assert next-object) ;; a value is required
      (bencode-dictionary-set obj key (decode-bencode-object next-object stream)))))

(defmethod decode-bencode-object ((obj bencode-list) (stream decode-stream))
  "Decode a bencoded list."
  (assert-next-char #\l stream)
  (do ((next-object (make-next-object stream)
                    (make-next-object stream))
       (lst nil))
      ((and (not next-object) (char= #\e (read-character stream)))
       (setf (bencode-object-value obj) (nreverse lst)))
    (assert next-object)
    (push (decode-bencode-object next-object stream) lst)))

(defun bencode-decode (buffer)
  (let* ((buffer (if (typep buffer 'stream)
                     (copy-stream-to-buffer buffer)
                     buffer))
         (stream (make-decode-stream buffer))
         (obj  (make-next-object stream)))
    (decode-bencode-object obj stream)))

(defun bencode-decode-file (pathname)
  "Decode the file named ``pathname''."
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (bencode-decode stream)))
