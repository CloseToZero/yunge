;;; yunge-reader-pdf-geometry.el --- PDF geometry helpers -*- lexical-binding: t; -*-
;; SPDX-FileCopyrightText: 2026 Chen Zhexuan
;; SPDX-License-Identifier: MIT

(require 'cl-lib)
(require 'subr-x)

(cl-defstruct yunge-reader-pdf--hit-index
  "One page-local spatial index for PDF character hit testing."
  source
  tolerance
  cell-size
  cells)


(defun yunge-reader-pdf--convex-quad-p (quad)
  "Return non-nil when QUAD is strictly convex and outline-ordered."
  (let ((orientation 0)
        (valid t))
    (dotimes (index 4)
      (let* ((first (nth index quad))
             (second (nth (mod (1+ index) 4) quad))
             (third (nth (mod (+ index 2) 4) quad))
             (cross
              (- (* (- (alist-get 'x second)
                       (alist-get 'x first))
                    (- (alist-get 'y third)
                       (alist-get 'y second)))
                 (* (- (alist-get 'y second)
                       (alist-get 'y first))
                    (- (alist-get 'x third)
                       (alist-get 'x second))))))
        (if (< (abs cross) 0.000001)
            (setq valid nil)
          (let ((sign (if (> cross 0) 1 -1)))
            (if (= orientation 0)
                (setq orientation sign)
              (unless (= orientation sign)
                (setq valid nil)))))))
    valid))

(defun yunge-reader-pdf--canonical-point-p (point)
  "Return non-nil when POINT is one strict canonical PDF point."
  (and (listp point)
       (equal (mapcar #'car-safe point) '(x y))
       (cl-every #'numberp (mapcar #'cdr point))))

(defun yunge-reader-pdf--quad-points (character)
  "Return CHARACTER's canonical quadrilateral, or nil when unavailable.
Signal an error when CHARACTER supplies a malformed non-nil quad."
  (let ((quad (alist-get 'quad character)))
    (cond
     ((null quad) nil)
     ((and (listp quad)
           (= (length quad) 4)
           (cl-every #'yunge-reader-pdf--canonical-point-p quad)
           (yunge-reader-pdf--convex-quad-p quad))
      quad)
     (t
      (error "Invalid PDF character quad: %S" quad)))))

(defun yunge-reader-pdf--svg-quad
    (quad page-width page-height pixel-width pixel-height)
  "Project canonical QUAD to SVG coordinates."
  (mapcar
   (lambda (point)
     (cons
      (* pixel-width
         (/ (alist-get 'x point) page-width))
      (* pixel-height
         (/ (- page-height (alist-get 'y point))
            page-height))))
   quad))

(defun yunge-reader-pdf--svg-bounds
    (bounds page-width page-height pixel-width pixel-height)
  "Project canonical BOUNDS to four SVG points."
  (let* ((left (alist-get 'left bounds))
         (bottom (alist-get 'bottom bounds))
         (right (alist-get 'right bounds))
         (top (alist-get 'top bounds))
         (x (* pixel-width (/ left page-width)))
         (y (* pixel-height (/ (- page-height top) page-height)))
         (width
          (max 1 (* pixel-width (/ (- right left) page-width))))
         (height
          (max 1 (* pixel-height (/ (- top bottom) page-height)))))
    (list (cons x y)
          (cons (+ x width) y)
          (cons (+ x width) (+ y height))
          (cons x (+ y height)))))

(defun yunge-reader-pdf--axis-aligned-quad-p (quad)
  "Return non-nil when every edge of canonical QUAD is axis-aligned."
  (let* ((xs (mapcar (lambda (point) (alist-get 'x point)) quad))
         (ys (mapcar (lambda (point) (alist-get 'y point)) quad))
         (span
          (max (- (apply #'max xs) (apply #'min xs))
               (- (apply #'max ys) (apply #'min ys))))
         (tolerance (max 0.000001 (* span 0.001)))
         (previous (car (last quad)))
         (aligned t))
    (dolist (current quad aligned)
      (unless
          (or (<= (abs (- (alist-get 'x previous)
                          (alist-get 'x current)))
                  tolerance)
              (<= (abs (- (alist-get 'y previous)
                          (alist-get 'y current)))
                  tolerance))
        (setq aligned nil))
      (setq previous current))))

(defun yunge-reader-pdf--highlight-bounds-height (bounds)
  "Return canonical height of highlight BOUNDS."
  (- (alist-get 'top bounds) (alist-get 'bottom bounds)))

(defun yunge-reader-pdf--same-highlight-run-p (left right)
  "Return non-nil when LEFT and RIGHT bounds form one text-line run."
  (let* ((left-height
          (yunge-reader-pdf--highlight-bounds-height left))
         (right-height
          (yunge-reader-pdf--highlight-bounds-height right))
         (overlap
          (- (min (alist-get 'top left) (alist-get 'top right))
             (max (alist-get 'bottom left)
                  (alist-get 'bottom right))))
         (gap
          (max 0.0
               (- (max (alist-get 'left left)
                       (alist-get 'left right))
                  (min (alist-get 'right left)
                       (alist-get 'right right))))))
    (and (> left-height 0)
         (> right-height 0)
         (>= overlap (* 0.5 (min left-height right-height)))
         (<= gap (* 0.75 (max left-height right-height))))))

(defun yunge-reader-pdf--merge-highlight-bounds (left right)
  "Return the bounding union of canonical bounds LEFT and RIGHT."
  `((left . ,(min (alist-get 'left left)
                  (alist-get 'left right)))
    (bottom . ,(min (alist-get 'bottom left)
                    (alist-get 'bottom right)))
    (right . ,(max (alist-get 'right left)
                   (alist-get 'right right)))
    (top . ,(max (alist-get 'top left)
                 (alist-get 'top right)))))

(defun yunge-reader-pdf--polygon-signed-area (points)
  "Return twice the signed area enclosed by SVG POINTS."
  (let ((area 0.0)
        (previous (car (last points))))
    (dolist (current points area)
      (cl-incf area
               (- (* (car previous) (cdr current))
                  (* (car current) (cdr previous))))
      (setq previous current))))

(defun yunge-reader-pdf--highlight-path-commands (points)
  "Return one consistently wound closed SVG subpath for POINTS."
  (when points
    (let ((points
           (if (< (yunge-reader-pdf--polygon-signed-area points) 0)
               (reverse points)
             points)))
      (list
       (list 'moveto (list (car points)))
       (list 'lineto (cdr points))
       (list 'closepath)))))

(defun yunge-reader-pdf--prepend-highlight-path (points commands)
  "Prepend POINTS as a closed subpath to reversed COMMANDS."
  (dolist (command
           (yunge-reader-pdf--highlight-path-commands points)
           commands)
    (push command commands)))


(defun yunge-reader-pdf--bounds-distance (x y bounds)
  "Return squared distance from canonical X and Y to BOUNDS."
  (let* ((left (alist-get 'left bounds))
         (bottom (alist-get 'bottom bounds))
         (right (alist-get 'right bounds))
         (top (alist-get 'top bounds))
         (dx (cond ((< x left) (- left x))
                   ((> x right) (- x right))
                   (t 0.0)))
         (dy (cond ((< y bottom) (- bottom y))
                   ((> y top) (- y top))
                   (t 0.0))))
    (+ (* dx dx) (* dy dy))))

(defun yunge-reader-pdf--segment-distance (x y start end)
  "Return squared distance from X and Y to segment START through END."
  (let* ((start-x (alist-get 'x start))
         (start-y (alist-get 'y start))
         (delta-x (- (alist-get 'x end) start-x))
         (delta-y (- (alist-get 'y end) start-y))
         (length-squared
          (+ (* delta-x delta-x) (* delta-y delta-y)))
         (ratio
          (if (> length-squared 0)
              (max 0.0
                   (min 1.0
                        (/ (+ (* (- x start-x) delta-x)
                              (* (- y start-y) delta-y))
                           length-squared)))
            0.0))
         (nearest-x (+ start-x (* ratio delta-x)))
         (nearest-y (+ start-y (* ratio delta-y)))
         (distance-x (- x nearest-x))
         (distance-y (- y nearest-y)))
    (+ (* distance-x distance-x)
       (* distance-y distance-y))))

(defun yunge-reader-pdf--quad-contains-p (x y quad)
  "Return non-nil when canonical point X and Y lies in convex QUAD."
  (let ((orientation 0)
        (inside t))
    (dotimes (index 4)
      (let* ((start (nth index quad))
             (end (nth (mod (1+ index) 4) quad))
             (cross
              (- (* (- (alist-get 'x end)
                       (alist-get 'x start))
                    (- y (alist-get 'y start)))
                 (* (- (alist-get 'y end)
                       (alist-get 'y start))
                    (- x (alist-get 'x start))))))
        (unless (< (abs cross) 0.000001)
          (let ((sign (if (> cross 0) 1 -1)))
            (if (= orientation 0)
                (setq orientation sign)
              (unless (= orientation sign)
                (setq inside nil)))))))
    inside))

(defun yunge-reader-pdf--quad-distance (x y quad)
  "Return squared distance from canonical X and Y to convex QUAD."
  (if (yunge-reader-pdf--quad-contains-p x y quad)
      0.0
    (let ((distance most-positive-fixnum))
      (dotimes (index 4)
        (setq distance
              (min
               distance
               (yunge-reader-pdf--segment-distance
                x y
                (nth index quad)
                (nth (mod (1+ index) 4) quad)))))
      distance)))

(defun yunge-reader-pdf--character-distance (x y character)
  "Return squared distance from X and Y to CHARACTER geometry."
  (if-let* ((quad (yunge-reader-pdf--quad-points character)))
      (yunge-reader-pdf--quad-distance x y quad)
    (when-let* ((bounds (alist-get 'bounds character)))
      (yunge-reader-pdf--bounds-distance x y bounds))))


(defun yunge-reader-pdf--selectable-character-p (character)
  "Return non-nil when CHARACTER can be selected."
  (and (not (alist-get 'generated character))
       (not
        (string-empty-p
         (or (alist-get 'text character) "")))))

(defun yunge-reader-pdf--character-index-bounds (character)
  "Return axis-aligned indexing bounds for CHARACTER, or nil."
  (if-let* ((quad (yunge-reader-pdf--quad-points character)))
      (let ((xs (mapcar (lambda (point) (alist-get 'x point)) quad))
            (ys (mapcar (lambda (point) (alist-get 'y point)) quad)))
        (list (apply #'min xs)
              (apply #'min ys)
              (apply #'max xs)
              (apply #'max ys)))
    (when-let* ((bounds (alist-get 'bounds character))
                (left (alist-get 'left bounds))
                (bottom (alist-get 'bottom bounds))
                (right (alist-get 'right bounds))
                (top (alist-get 'top bounds))
                ((cl-every #'numberp (list left bottom right top)))
                ((<= left right))
                ((<= bottom top)))
      (list left bottom right top))))


(provide 'yunge-reader-pdf-geometry)

;;; yunge-reader-pdf-geometry.el ends here
