(in-package :cl-torrent.file-store)

(defun check-range-params (start end len)
  (cond
    ((and (not start) end len)
     (setq start (- end len)))
    ((and (not end) start len)
     (setq end (+ start len)))
    ((and (not len) start end)
     (setq len (- end start))))
  (assert (and start end len)
          (start end len)
          "invalid parameters ~d ~d ~d" start end len)
  (assert (and (>= start 0) (>= end start) (>= len 0))
          (start end len)
          "invalid parameters ~d ~d ~d" start end len)
  (assert (= (+ start len) end)
          (start end len))
  (values start end len))

(defstruct (range (:constructor make-range%))
  (start  nil :read-only t)
  (end    nil :read-only t)
  (length nil :read-only t))

(defun make-range (&optional &key start end length)
  (multiple-value-bind (start end length)
      (check-range-params start end length)
    (make-range% :start start :end end :length length)))

;; (defun range-from-bounds (start end)
;;   (make-range :start start :end end))
;; (defun range-to-bounds (range)
;;   (cons (range-start range) (range-end range)))
;; (defun range-set-to-bounds (rset)
;;   (map #'range-to-bounds (range-ranges rset)))

(defstruct (range-set (:constructor make-range-set%))
  (start  nil :read-only t)
  (end    nil :read-only t)
  (length nil :read-only t)
  (ranges nil :read-only t))

(defun make-range-set (&optional &key start end length ranges)
  (multiple-value-bind (start end length)
      (check-range-params start end length)
    (make-range-set% :start  start
                     :end    end
                     :length length
                     :ranges (or ranges
                                 (list (make-range :start start :end end))))))

;; much cleaner way to do this:
;; (defun range-set-union (seta setb))
;; (defun range-set-intersection (seta setb))
;; (defun range-set-negate (seta))

;; but handcoded is faster.
(defun range-set-fit (range-set range)
  (let ((start  (range-set-start  range-set))
        (end    (range-set-end    range-set))
        (ranges (range-set-ranges range-set))
        (new-range range)
        holes
        filler)
    (loop
       (unless ranges
         (return (values (nreverse filler)
                         (make-range-set :start  start
                                         :end    end
                                         :ranges (nreverse holes)))))
       (let* ((cur-range (car ranges))
              (x0     (range-start cur-range))
              (y0     (range-end   cur-range))
              (x1     (range-start new-range))
              (y1     (range-end   new-range)))
         (setf ranges (cdr ranges))
         (macrolet ((push-range (x y lst)
                      `(unless (= ,x ,y)
                         (push (make-range :start ,x :end ,y) ,lst))))
           (cond
             ((or (> x1 y0) (< y1 x0))
              (push cur-range holes))
             ((and (>= x1 x0) (<= y1 y0)) ; x0 <= x1 < y1 <= y0
              (push-range x0 x1 holes)
              (push-range y1 y0 holes)
              (push-range x1 y1 filler))
             ((and (<= x1 x0) (<= y1 y0)) ; x1 <= x0 < y1 <= y0
              (push-range y1 y0 holes)
              (push-range x0 y1 filler))
             ((and (>= x1 x0) (>= y1 y0)) ; x0 <= x1 < y0 <= y1
              (push-range x0 x1 holes)
              (push-range x1 y0 filler)
              (unless (= x1 y0)
                (setq new-range (make-range :start y0 :end y1))))
             ((and (<= x1 x0) (>= y1 y0)) ; x1 <= x0 < y0 <= y1
              (push-range x1 y0 holes)
              (push-range x1 x0 filler)
              (unless (= x0 y0)
                (setq new-range (make-range :start y0 :end y1))))))))))
