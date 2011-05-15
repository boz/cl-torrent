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

(defmethod load-torrent ((loader torrent-loader) (metainfo metainfo))
  (let ((dl-cache (dl-cache-dir loader (info-hash metainfo))))
    (when (directory-exists-p dl-cache)
      (error "torrent exists"))
    (initialize-dl-cache cache-dir)
    (make-torrent-download :cache-dir    dl-cache
                           :download-dir (download-dir loader)
                           :metainfo     metainfo)))

