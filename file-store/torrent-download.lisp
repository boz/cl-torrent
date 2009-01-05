(in-package :cl-torrent.file-store)

(defclass torrent-download ()
  ((cache-dir    :reader cache-dir    :initarg :cache-dir)
   (download-dir :reader download-dir :initarg :download-dir)
   (files        :reader files)
   (pieces       :reader pieces)))

(defgeneric initialize-torrent (torrent-download metainfo torrent-path))
(defgeneric save-state (torrent-download))
(defgeneric load-state (torrent-download))

(defgeneric set-download-path (torrent-download path))
(defgeneric set-file-path     (torrent-download index path))
(defgeneric set-file-priority (torrent-download index pri))

(defmethod initialize-torrent ((dl torrent-download) metainfo torrent-path)
  (initialize-dl-cache (cache-dir dl) torrent-path)
  
  )