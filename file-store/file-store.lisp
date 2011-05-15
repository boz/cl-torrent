(in-package :cl-torrent.file-store)

(defstruct piece-spec
  index have size hash files)

(defstruct file-slice
  file start end)

(defstruct piece-slice
  piece start end)

(defstruct file-spec
  index size path md5 piece-begin piece-end)

(defgeneric make-store-state (info-dict))
(defmethod  make-store-state ((info info-dict-single)))

(defun info-hash->pieces (pieces)
  (iter (for x in-sequence pieces :with-index idx)
        (collect (make-piece-spec :index      idx
                                  :piece-size 0
                                  :hash       x)
          :result-type vector)))

(defun info-hash->files (files dir)
  (iter (for x in-sequence files :with-index idx)
        (collect (make-file-spec :index idx
                                 :size  (info-piece-length x)
                                 :path  (cons dir (info-file-path x))
                                 :md5   (info-file-md5 x))
          :result-type vector)))


(defmethod make-store-state ((info info-dict-multi))
  (with-accessors ((piece-len  info-piece-length)
                   (pieces     info-pieces)
                   (files      info-filess)
                   (name       info-name))
      info
    (let ((pieces (initialize-pieces pieces))
          (files  (initialize-files  files name)))
      (initialize-piece-file-map files pieces piece-length))))

(defmethod make-store-state ((info info-dict-single))
  )

(defun initialize-piece-file-map (files pieces piece-length)
  (iter
    (with pieceidx = 0)
    (for file in-vector files :with-index findx)
    (finally (values pieces files))
    (iter
      (for piece in-vector pieces upfrom pieceidx)
      (after-each (incf pieceidx))
      
      (for cursz = (piece-spec-size piece))
      (for room  = (- piece-len cursz))
      (for delta = (min bytes-left room))
      
      (when (zerop room)
        (next-iteration))
      
      (decf bytes-left delta)
      (incf (piece-spec-size piece) delta)
      (push (piece-spec-files piece)
            (make-file-slice :file findx 
                             :start (- bytes-left delta) 
                             :end bytes-left))

      (flet ((mkpslice (idx cursz delta)
               (make-file-piece-boundry :piece idx
                                        :start (- cursz delta)
                                        :end cursz)))
        (when (not (file-spec-piece-begin file))
          (setf (file-spec-piece-begin file)
                (mkpslice pieceidx cursz delta)))
        (when (zerop bytes-left)
          (setf (file-spec-piece-end file)
                (mkpslice pieceidx cursz delta))
          (finish))))))
