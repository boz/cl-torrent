(in-package :cl-torrent.file-store)

(defun info-hash-hex (info-hash)
  (byte-array-to-hex-string info-hash))

(defun dl-cache-dir (loader info-hash)
  (pathname-as-directory
   (path-add-basename (cache-dir loader) (info-hash-hex hash-str))))

(defclass torrent-loader ()
  ((cache-dir    :accessor cache-dir    :initarg :cache-dir)
   (download-dir :accessor download-dir :initarg :download-dir)))

(defgeneric initialize-loader (loader))
(defgeneric load-torrent (loader path))

(defmethod initialize-loader ((loader torrent-loader))
  (ensure-directory-exists (cache-dir loader)))

(defmethod load-torrent ((loader torrent-loader) path)
  (multiple-value-bind (metainfo buffer)
      (metainfo-decode-file path)
    (let ((dl-cache (dl-cache-dir loader (info-hash metainfo))))
      (when (directory-exists-p dl-cache)
        (error "torrent exists"))
      (dl (make-instance 'torrent-download
                         :cache-dir    dl-cache
                         :download-dir (download-dir loader)))
      (initialize-torrent metainfo path)
      dl)))

(defclass download-config ()
  ((cache-dir
    :accessor cache-dir :initarg :cache-dir)
   (download-directory
    :accessor download-directory :initarg :download-directory)
   (files
    :reader files :initarg :files)
   (metainfo
    :reader metainfo :initarg :metainfo)
   (metainfo-bytes
    :reader metainfo-bytes :initarg :metainfo-bytes)))
