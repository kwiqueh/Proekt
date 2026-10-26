;;; GeoCAD Tools 0.1. Pure list geometry; no drawing API.
;;; Settings: drawing units; output precision; GEO axis mapping (example only).
(setq gct:duplicate-tolerance 0.001
      gct:output-precision 3
      gct:geo-swap T)

;;; Two 2D points -> Euclidean distance (Z intentionally ignored).
(defun gct:dist2d (a b / dx dy)
  (setq dx (- (car b) (car a)) dy (- (cadr b) (cadr a)))
  (sqrt (+ (* dx dx) (* dy dy))))

;;; Point list -> closed ring edge lengths, including last-to-first.
(defun gct:segment-lengths (pts / rest result)
  (setq rest pts result nil)
  (if (> (length pts) 1)
    (progn
      (while (cdr rest)
        (setq result (cons (gct:dist2d (car rest) (cadr rest)) result)
              rest (cdr rest)))
      (setq result (cons (gct:dist2d (car rest) (car pts)) result))))
  (reverse result))

;;; Point list -> closed perimeter in drawing units.
(defun gct:perimeter (pts / total x)
  (setq total 0.0)
  (foreach x (gct:segment-lengths pts) (setq total (+ total x)))
  total)

;;; Point list -> signed shoelace area. Translate to first vertex to reduce cancellation.
(defun gct:area-gauss (pts / origin rest a b total)
  (setq total 0.0 origin (car pts) rest (cdr pts))
  (while (cdr rest)
    (setq a (car rest) b (cadr rest)
          total (+ total (- (* (- (car a) (car origin)) (- (cadr b) (cadr origin)))
                            (* (- (cadr a) (cadr origin)) (- (car b) (car origin)))))
          rest (cdr rest)))
  (/ total 2.0))

;;; Points -> CCW, CW or DEGENERATE, relative to supplied XY axes.
(defun gct:orientation (pts / area)
  (setq area (gct:area-gauss pts))
  (cond ((> area 0.0) "CCW") ((< area 0.0) "CW") (T "DEGENERATE")))

;;; Points and nonnegative tolerance -> list of 1-based duplicate index pairs.
(defun gct:find-duplicates (pts tol / rest tail i j result)
  (setq rest pts i 1 result nil)
  (while rest
    (setq tail (cdr rest) j (+ i 1))
    (while tail
      (if (<= (gct:dist2d (car rest) (car tail)) tol)
        (setq result (cons (list i j) result)))
      (setq tail (cdr tail) j (+ j 1)))
    (setq rest (cdr rest) i (+ i 1)))
  (reverse result))

;;; Three points -> oriented cross product of AB and AC.
(defun gct:cross (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a)))))

;;; Point and segment with tolerance -> T if within tolerance of segment.
(defun gct:on-segment (p a b tol / len)
  (setq len (gct:dist2d a b))
  (and (<= (abs (gct:cross a b p)) (* tol len))
       (<= (- (min (car a) (car b)) tol) (car p) (+ (max (car a) (car b)) tol))
       (<= (- (min (cadr a) (cadr b)) tol) (cadr p) (+ (max (cadr a) (cadr b)) tol))))

;;; Two segments and tolerance -> T for crossing, touching or near-touching endpoints.
(defun gct:intersects (a b c d tol / ab1 ab2 cd1 cd2)
  (setq ab1 (gct:cross a b c) ab2 (gct:cross a b d)
        cd1 (gct:cross c d a) cd2 (gct:cross c d b))
  (or (and (< (* ab1 ab2) 0.0) (< (* cd1 cd2) 0.0))
      (gct:on-segment a c d tol) (gct:on-segment b c d tol)
      (gct:on-segment c a b tol) (gct:on-segment d a b tol)))

;;; Closed ring and tolerance -> T if nonadjacent sides intersect or adjacent sides backtrack.
(defun gct:self-intersects (pts tol / edges rest i j n hit a b c d)
  (setq edges (append pts (list (car pts))) rest edges i 0 n (length pts) hit nil)
  (while (< i n)
    (setq a (nth i edges) b (nth (+ i 1) edges) j (+ i 1))
    (while (< j n)
      (setq c (nth j edges) d (nth (+ j 1) edges))
      (cond
        ((= j (+ i 1))
          (if (or (gct:on-segment a c d tol) (gct:on-segment d a b tol)) (setq hit T)))
        ((and (= i 0) (= j (- n 1)))
          (if (or (gct:on-segment b c d tol) (gct:on-segment c a b tol)) (setq hit T)))
        ((gct:intersects a b c d tol) (setq hit T)))
      (setq j (+ j 1)))
    (setq i (+ i 1)))
  hit)

;;; Point list -> T when every point contains numeric XY, otherwise nil.
(defun gct:points-valid (pts / valid p)
  (setq valid (listp pts))
  (if valid
    (foreach p pts
      (if (not (and (listp p) (numberp (car p)) (numberp (cadr p))))
        (setq valid nil))))
  valid)

;;; Numeric point list and tolerance -> T if coordinate scale threatens tolerance resolution.
;;; Conservative double-precision guard; no coordinate changes or rounding.
(defun gct:precision-risk (pts tol / risk p)
  (setq risk nil)
  (foreach p pts
    (if (> (* 1e-14 (max (abs (car p)) (abs (cadr p)))) tol) (setq risk T)))
  risk)

;;; Points, closed flag, arc flag, tolerance -> warning list; any warning blocks calculation.
;;; Invalid numeric input/tolerance returns a warning instead of calling geometry functions.
(defun gct:validate (pts closed arcs tol)
  (cond
    ((not (and (numberp tol) (> tol 0.0))) (list "Некорректный допуск: требуется положительное число."))
    ((not (gct:points-valid pts)) (list "Некорректный список числовых XY-вершин."))
    (T (gct:validate-ring pts closed arcs tol))))

;;; Valid numeric XY ring and positive tolerance -> geometric warning list.
(defun gct:validate-ring (pts closed arcs tol / warnings)
  (setq warnings nil)
  (if (gct:precision-risk pts tol)
    (setq warnings (cons "Масштаб координат слишком велик для надёжной проверки с данным допуском." warnings)))
  (if (not closed) (setq warnings (cons "Контур не замкнут (DXF 70)." warnings)))
  (if arcs (setq warnings (cons "контур содержит дуги, расчёт не выполнен" warnings)))
  (if (< (length pts) 3) (setq warnings (cons "Менее трёх вершин." warnings)))
  (if (gct:find-duplicates pts tol)
    (setq warnings (cons "Повторяющиеся вершины в пределах допуска." warnings)))
  (if (>= (length pts) 3)
    (progn
      (if (gct:self-intersects pts tol)
        (setq warnings (cons "Контур некорректен: пересечение, касание или возврат сторон." warnings)))
      (if (<= (abs (gct:area-gauss pts)) (* tol (gct:perimeter pts)))
        (setq warnings (cons "Вырожденный или слишком узкий контур относительно допуска." warnings)))))
  (reverse warnings))
(princ)
