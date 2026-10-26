;;; GeoCAD Tools 0.1. Load gct-core.lsp first. No Visual LISP/COM.

;;; Number -> display only, with explicit decimal mode.
(defun gct:fmt (x) (rtos x 2 gct:output-precision))

;;; DXF point value -> T if it contains numeric X and Y.
(defun gct:xy-p (p)
  (and (listp p) (numberp (car p)) (numberp (cadr p))))

;;; Entity -> (OCS-points WCS-points closed arcs warnings normal elevation).
;;; trans uses the entity's OCS (including DXF 210); elevation is supplied explicitly.
(defun gct:read-polyline (entity / data item pts world closed arcs warnings normal elevation p count)
  (setq data (entget entity) pts nil world nil arcs nil warnings nil
        normal (cdr (assoc 210 data)) elevation (cdr (assoc 38 data)))
  (if (not normal) (setq normal '(0.0 0.0 1.0)))
  (if (not elevation) (setq elevation 0.0))
  (setq closed (/= 0 (logand 1 (cdr (assoc 70 data)))))
  (foreach item data
    (cond
      ((= (car item) 10)
        (if (gct:xy-p (cdr item))
          (setq pts (cons (list (cadr item) (caddr item)) pts))
          (setq warnings (cons "Некорректные координаты DXF 10." warnings))))
      ((= (car item) 42)
        (if (or (not (numberp (cdr item))) (/= (cdr item) 0.0)) (setq arcs T)))))
  (setq pts (reverse pts) count (cdr (assoc 90 data)))
  (if (or (not (numberp count)) (/= count (length pts)))
    (setq warnings (cons "Число вершин не соответствует DXF 90." warnings)))
  (if (or (not (numberp elevation))
          (not (gct:xy-p normal)) (not (numberp (caddr normal)))
          (equal normal '(0.0 0.0 0.0) 1e-14))
    (setq warnings (cons "Некорректная высота или нормаль OCS." warnings)))
  (if (not warnings)
    (foreach p pts
      (setq world (cons (trans (list (car p) (cadr p) elevation) entity 0) world))))
  (list pts (reverse world) closed arcs (reverse warnings) normal elevation))

;;; Parsed entity record -> printed report; returns nil. Does not modify drawing.
(defun gct:report (record / pts world warnings p gx gy i lengths value normal)
  (setq pts (nth 0 record) world (nth 1 record) normal (nth 5 record)
        warnings (append (nth 4 record)
                    (gct:validate pts (nth 2 record) (nth 3 record) gct:duplicate-tolerance)))
  (princ "\nGeoCAD Tools 0.1 — координаты WCS; длины и площадь в плоскости OCS.")
  (princ (if gct:geo-swap "\nGEO: X=CAD_Y, Y=CAD_X (настройка примера)." "\nGEO: X=CAD_X, Y=CAD_Y."))
  (princ (strcat "\nДопуск: " (gct:fmt gct:duplicate-tolerance) "; единицы чертежа, без пересчёта масштаба."))
  (if (and (gct:xy-p normal) (numberp (caddr normal)))
    (princ (strcat "\nНормаль WCS: " (gct:fmt (car normal)) ", "
    (gct:fmt (cadr normal)) ", " (gct:fmt (caddr normal)))))
  (princ "\n№ | CAD_X | CAD_Y | CAD_Z | Geo_X | Geo_Y")
  (setq i 1)
  (foreach p world
    (setq gx (if gct:geo-swap (cadr p) (car p)) gy (if gct:geo-swap (car p) (cadr p)))
    (princ (strcat "\n" (itoa i) " | " (gct:fmt (car p)) " | " (gct:fmt (cadr p))
      " | " (gct:fmt (caddr p)) " | " (gct:fmt gx) " | " (gct:fmt gy)))
    (setq i (+ i 1)))
  (if warnings
    (progn
      (princ "\nПРЕДУПРЕЖДЕНИЯ — расчёт не выполнен:")
      (foreach value warnings (princ (strcat "\n- " value))))
    (progn
      (setq lengths (gct:segment-lengths pts) i 1)
      (foreach value lengths
        (princ (strcat "\nСторона " (itoa i) " → " (itoa (if (= i (length pts)) 1 (+ i 1))) ": " (gct:fmt value)))
        (setq i (+ i 1)))
      (princ (strcat "\nПериметр: " (gct:fmt (gct:perimeter pts))
        "\nПлощадь: " (gct:fmt (abs (gct:area-gauss pts)))
        "\nОбход в OCS (вид со стороны +нормали): "
        (if (= (gct:orientation pts) "CCW") "против часовой" "по часовой")
        "\nПредупреждения: нет. Ширина полилинии игнорируется; расчёт по осевой линии."))))
  (princ))

;;; GCT_INFO: select one LWPOLYLINE, validate and print; handles cancellation/errors without partial totals.
(defun c:GCT_INFO (/ *error* selected entity data)
  (defun *error* (message)
    (princ (strcat "\nGCT_INFO: расчёт прерван, результат не подтверждён: " message)) (princ))
  (cond
    ((or (not (numberp gct:duplicate-tolerance)) (<= gct:duplicate-tolerance 0.0)
         (not (numberp gct:output-precision)) (< gct:output-precision 0)
         (> gct:output-precision 8) (/= gct:output-precision (fix gct:output-precision)))
      (princ "\nНекорректные настройки допуска или точности (целое 0..8)."))
    ((not (setq selected (entsel "\nВыберите одну замкнутую LWPOLYLINE: ")))
      (princ "\nВыбор отменён."))
    (T
      (setq entity (car selected) data (entget entity))
      (if (= (cdr (assoc 0 data)) "LWPOLYLINE")
        (gct:report (gct:read-polyline entity))
        (princ "\nТребуется LWPOLYLINE; объект не обработан."))))
  (princ))
(princ "\nGeoCAD Tools 0.1: команда GCT_INFO.")
(princ)
