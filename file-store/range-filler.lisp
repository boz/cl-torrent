(in-package :cl-torrent.file-store)

(defun make-range-filler (size &key (start 0))
  (list (cons start size)))

(defun range-filled? (ranges)
  (not ranges))

(defun fill-range (ranges x1 y1)
  (let (holes filler)
    (loop
       (unless ranges
         (return (values (reverse filler) (reverse holes))))
       (let* ((range (car ranges))
              (x0    (car range))
              (y0    (cdr range)))
         (setf ranges (cdr ranges))
         (cond
           ((or (> x1 y0) (< y1 x0))
            (push range holes))
           ((and (>= x1 x0) (<= y1 y0)) ; x0 <= x1 < y1 <= y0
            (unless (= x0 x1) (push (cons x0 x1) holes))
            (unless (= y1 y0) (push (cons y1 y0) holes))
            (unless (= x1 y1) (push (cons x1 y1) filler)))
           ((and (<= x1 x0) (<= y1 y0)) ; x1 <= x0 < y1 <= y0
            (unless (= y1 y0) (push (cons y1 y0) holes))
            (unless (= x0 y1) (push (cons x0 y1) filler)))
           ((and (>= x1 x0) (>= y1 y0)) ; x0 <= x1 < y0 <= y1
            (unless (= x0 x1) (push (cons x0 x1) holes))
            (unless (= x1 y0) (push (cons x1 y0) filler))
            (unless (= x1 y0) (setf x1 y0)))
           ((and (<= x1 x0) (>= y1 y0)) ; x1 <= x0 < y0 <= y1
            (unless (= x0 y0) (push (cons x0 y0) filler))
            (unless (= x1 y0) (setf x1 y0))))))))

(defun range->offset-size (x)
  (cons (car x) (- (cdr x) (car x))))

(defun offset-size->range (x)
  (cons (car x) (+ (cdr x) (car x))))
