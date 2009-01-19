(in-package :cl-torrent-test)

(deftestsuite test-file-store-ranges (cl-torrent-test-suite)
  ((invalid-params
    '((:start  10)
      (:end    10)
      (:length 10)
      (:start  10 :end 5)
      (:start  10 :length -2)
      (:end    5  :length 10)
      (:start  2  :end    5 :length 10)))

   (range-set-fit-tests
    '(

      (((0  10))         ;; initial set
       (5 6)             ;; inserted range
       ((5  6))          ;; inserted list result
       ((0  5) (6  10))) ;; resulting set

      (((0 5) (6 10))
       (5 6)
       ()
       ((0 5) (6 10)))

      (((0 10))
       (14 15)
       ()
       ((0 10)))

      ;; broken.
;;       (((5 7))
;;        (3 9)
;;        ((3 5) (7 9))
;;        ((3 9)))

      ))))

(addtest (test-file-store-ranges)
  test-make-range
  (let ((x (make-range :start 0 :end 5)))
    (ensure-same 5 (range-length x))
    (ensure-same 0 (range-start x))
    (ensure-same 5 (range-end x)))
  (dolist (i invalid-params)
    (ensure-error (apply #'make-range i))))

(addtest (test-file-store-ranges)
  test-make-range-store
  (dolist (i invalid-params)
    (ensure-error (apply #'make-range-set i))))

(defun range-from-bounds (start end)
  (make-range :start start :end end))
(defun range-to-bounds (range)
  (list (range-start range) (range-end range)))
(defun range-set-to-bounds (rset)
  (mapcar #'range-to-bounds (range-set-ranges rset)))

(defun set-from-bounds (bounds)
  (assert bounds)
  (let* ((ranges (mapcar #'(lambda (x) (apply #'range-from-bounds x)) bounds))
         (start  (range-start (car ranges)))
         (end    (range-end   (car (reverse ranges)))))
    (make-range-set :start start :end end :ranges ranges)))

(addtest (test-file-store-ranges)
  test-range-set-fit
  (dolist (desc range-set-fit-tests)
    (destructuring-bind
          (set-spec (nrstart nrend) einserted enew-set)
        desc
      (let ((set (set-from-bounds set-spec))
            (newrange (range-from-bounds nrstart nrend)))
        (multiple-value-bind (inserted new-set)
            (range-set-fit set newrange)
          (ensure-same einserted (mapcar #'range-to-bounds inserted))
          (ensure-same enew-set  (range-set-to-bounds new-set)))))))

