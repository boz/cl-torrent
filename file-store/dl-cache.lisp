(in-package :cl-torrent.file-store)

(defparameter *dl-cache-torrent-name* "orig")
(defparameter *dl-cache-state-name*   "state")
(defparameter *dl-cache-pieces-dir*   "pieces")

(defun dl-cache-state-path (cache-dir)
  (make-pathname :directory (pathname-directory cache-dir)
                 :name      *dl-cache-state-name*
                 :type      "lisp"))

(defun dl-cache-pieces-path (cache-dir)
  (pathname-as-directory
   (make-pathname :directory (pathname-directory cache-dir)
                  :name      *dl-cache-pieces-name*)))

(defun dl-cache-torrent-path (cache-dir)
  (make-pathname :directory (pathname-directory cache-dir)
                 :name      *dl-cache-torrent-name*
                 :type      "torrent"))

(defun initialize-dl-cache (cache-dir)
  (make-directory cache-dir)
  (let ((save-path   (dl-cache-torrent-path cache-dir))
        (pieces-path (dl-cache-pieces-path  cache-dir)))
    (metainfo-encode-file metainfo save-path)
    (make-directory pieces-path)))
