(in-package :cl-torrent.utils)

(defun user-homedir ()
  (let ((dir (sb-posix:getenv "HOME")))
    (when dir
      (pathname-as-directory dir))))

(defun make-directory (path &optional (mode #o755))
  (sb-posix:mkdir path mode))

(defun ensure-directory-exists (path &key (force t))
  (declare (ignore force))
  (unless (directory-exists-p path)
    ;; todo: condition.
    (make-directory path)))

(defun path-add-basename (dirname basename)
  (let ((dirname (pathname-as-directory dirname)))
    (make-pathname :directory (pathname-directory dirname)
                   :name basename)))

