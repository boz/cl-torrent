(in-package :cl-torrent.client)

(defpackage :cl-torrent.client
  (:use :cl :cl-torrent.utils)
  (:import-from :ironclad :byte-array-to-hex-string)

  (:export :configuration
           :cache-dir
           :download-dir
           :client-id
           :port))