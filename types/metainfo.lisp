(declaim (optimize (speed 0) (safety 3) (debug 3)))
(in-package :cl-torrent.types)

(define-bencmap-class metainfo ()
  ((info          :info-dict  :required t)
   (announce      :string     :required t)
   (creation-date :epoch-date :key "creation date")
   (created-by    :string     :key "created by")
   (info-hash     :info-hash)
   (announce-list :announce-list)
   (comment       :string)))

(define-bencmap-class info-dict-single ()
  ((piece-length :integer    :required t :key "piece length")
   (byte-length  :integer    :required t :key "length")
   (pieces       :piece-list :required t)
   (name         :string     :required t)
   (private      :integer)
   (md5sum       :string)))

(define-bencmap-class info-dict-multi ()
  ((piece-length :integer        :required t :key "piece length")
   (pieces       :piece-list     :required t)
   (files        :info-file-list :required t)
   (name         :string         :required t)
   (private      :integer)))

(define-bencmap-class info-dict-multi-file ()
  ((byte-length :integer     :required t :key "length")
   (path        :string-list :required t)
   (md5sum      :string)))

(defgeneric is-multi-file (info))
(defmethod  is-multi-file ((info info-dict-multi))  t)
(defmethod  is-multi-file ((info info-dict-single)) nil)

(defmethod decode-bencmap-value ((type (eql :info-file-list)) value ctx)
  (declare (ignore type))
  (when value
    (assert (listp value))
    (loop
       for x in value
       collect (decode-bencmap 'info-dict-multi-file (new-decode-ctx ctx x)))))

(defmethod decode-bencmap-value ((type (eql :announce-list)) value ctx)
  "http://www.bittornado.com/docs/multitracker-spec.txt"
  (declare (ignore type ctx))
  (when value
    (assert (listp value))
    (loop for x in value collect (mapcar #'octets->string x))))

(defmethod decode-bencmap-value ((type (eql :piece-list)) value ctx)
  (declare (ignore type ctx))
  (when value
    (loop
       repeat (/ (length value) 20)
       for x = 0 then (+ x 20)
       collect (subseq value x (+ x 20)))))

(defmethod decode-bencmap-value ((type (eql :info-dict)) value ctx)
  (declare (ignore type))
  (when value
    (assert (hash-table-p value))
    (let ((ctx (new-decode-ctx ctx value)))
      (if (decode-ctx-contains ctx "files")
          (decode-bencmap 'info-dict-multi ctx)
          (decode-bencmap 'info-dict-single ctx)))))

(defmethod decode-bencmap-value ((type (eql :info-hash)) value ctx)
  (declare (ignore type))
  (assert (not value))
  (multiple-value-bind (bytes exists)
      (decode-ctx-get-bytes ctx "info")
    (assert exists)
    (ironclad:byte-array-to-hex-string
     (ironclad:digest-sequence :sha1 bytes))))

(defun metainfo-decode (obj)
  (bencmap-decode obj 'metainfo))

(defun metainfo-decode-file (pathname)
  (bencmap-decode-file pathname 'metainfo))
