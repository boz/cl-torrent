(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.types)

(define-bencmap-class metainfo ()
  ((metainfo-info          :info-dict     :required t :key "info")
   (metainfo-announce      :string        :required t :key "announce")
   (metainfo-creation-date :epoch-date    :key "creation date")
   (metainfo-created-by    :string        :key "created by")
   (metainfo-info-hash     :info-hash     :key "info-hash")
   (metainfo-announce-list :announce-list :key "announce-list")
   (metainfo-comment       :string        :key "comment")))

(define-bencmap-class info-dict-single ()
  ((info-piece-length :integer    :required t :key "piece length")
   (info-byte-length  :integer    :required t :key "length")
   (info-pieces       :piece-list :required t :key "pieces")
   (info-name         :string     :required t :key "name")
   (info-private      :integer    :key "private")
   (info-md5sum       :string     :key "md5sum")))

(define-bencmap-class info-dict-multi ()
  ((info-piece-length :integer        :required t :key "piece length")
   (info-pieces       :piece-list     :required t :key "pieces")
   (info-files        :info-file-list :required t :key "files")
   (info-name         :string         :required t :key "name")
   (info-private      :integer        :key "private")))

(define-bencmap-class info-dict-multi-file ()
  ((info-file-byte-length :integer     :required t :key "length")
   (info-file-path        :string-list :required t :key "path")
   (info-file-md5sum      :string      :key "md5sum")))

(defgeneric is-multi-file (info))
(defmethod  is-multi-file ((info info-dict-multi))  t)
(defmethod  is-multi-file ((info info-dict-single)) nil)

(defmethod decode-bencmap-value ((type (eql :info-file-list)) value dict)
  (declare (ignore type))
  (with-bencode-value (value value)
    (assert (listp value))
    (loop
       for x in value
       collect (decode-bencmap 'info-dict-multi-file x))))

(defmethod decode-bencmap-value ((type (eql :announce-list)) value dict)
  "http://www.bittornado.com/docs/multitracker-spec.txt"
  (declare (ignore type dict))
  (with-bencode-value (value value)
    (assert (listp value))
    (loop for x in value collect (mapcar #'bencode-string-value (bencode-object-value x)))))

(defmethod decode-bencmap-value ((type (eql :piece-list)) value dict)
  (declare (ignore type dict))
  (with-bencode-value (value value)
    (loop
       repeat (/ (length value) 20)
       for x = 0 then (+ x 20)
       collect (subseq value x (+ x 20)))))

(defmethod decode-bencmap-value ((type (eql :info-dict)) value dict)
  (declare (ignore type))
  (when value
    (assert (typep value 'bencode-dictionary))
    (if (bencode-dictionary-get value "files")
        (decode-bencmap 'info-dict-multi  value)
        (decode-bencmap 'info-dict-single value))))

(defmethod decode-bencmap-value ((type (eql :info-hash)) value dict)
  (declare (ignore type))
  (assert (not value))
  (let ((info (bencode-dictionary-get dict "info")))
    (assert info)
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 (bencode-object-bytes info)))))

(defun metainfo-decode (obj)
  (bencmap-decode obj 'metainfo))

(defun metainfo-decode-file (pathname)
  (bencmap-decode-file pathname 'metainfo))
