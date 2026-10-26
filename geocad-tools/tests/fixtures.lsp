;;; Native CAD fixtures. Load after both src files. Creates geometry only when requested.

;;; Points, closure, bulge, normal, elevation -> new LWPOLYLINE entity name or nil.
(defun gct:test-make (pts closed bulge normal elevation / data p index)
  (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
    '(100 . "AcDbPolyline") (cons 90 (length pts)) (cons 70 closed)
    (cons 38 elevation)) index 0)
  (foreach p pts
    (setq data (append data (list (cons 10 p) (cons 42 (if (= index 0) bulge 0.0))))
          index (+ index 1)))
  (setq data (append data (list (cons 210 normal))))
  (if (entmake data) (entlast) nil))

;;; Interactive fixture number -> creates a test polyline and prints its computed report.
(defun c:GCT_FIXTURE (/ number pts closed bulge normal elevation entity)
  (initget 1)
  (setq number (getint "\nНомер теста 1..7: ")
        pts '((0.0 0.0) (10.0 0.0) (10.0 20.0) (0.0 20.0))
        closed 1 bulge 0.0 normal '(0.0 0.0 1.0) elevation 0.0)
  (cond
    ((= number 2) (setq pts (reverse pts)))
    ((= number 3) (setq pts '((0.0 0.0) (10.0 0.0) (10.0 0.0) (10.0 20.0) (0.0 20.0))))
    ((= number 4) (setq closed 0))
    ((= number 5) (setq pts '((0.0 0.0) (10.0 20.0) (0.0 20.0) (10.0 0.0))))
    ((= number 6) (setq normal '(0.0 1.0 0.0) elevation 7.0))
    ((= number 7) (setq bulge 0.5)))
  (if (and (>= number 1) (<= number 7))
    (progn
      (setq entity (gct:test-make pts closed bulge normal elevation))
      (if entity
        (progn (princ "\nТестовая полилиния создана.") (gct:report (gct:read-polyline entity)))
        (princ "\nОШИБКА: CAD не создал тестовый объект.")))
    (princ "\nТребуется номер 1..7."))
  (princ))

;;; Actual native trans check, including changed UCS; no changes to UCS by this function.
;;; Creates one fixture; expected WCS values come from the OCS arbitrary-axis construction.
(defun c:GCT_CHECK6 (/ entity record expected actual ok)
  (setq entity (gct:test-make '((0.0 0.0) (10.0 0.0) (10.0 20.0) (0.0 20.0))
                 1 0.0 '(0.0 1.0 0.0) 7.0)
        expected '((0.0 7.0 0.0) (-10.0 7.0 0.0) (-10.0 7.0 20.0) (0.0 7.0 20.0)))
  (if entity
    (progn
      (setq record (gct:read-polyline entity) actual (nth 1 record)
            ok (and (not (nth 4 record)) (equal actual expected 1e-9)))
      (princ (if ok "\nPASS: test 6 WCS (trans)." "\nFAIL: test 6 WCS (trans)."))
      (gct:report record))
    (princ "\nFAIL: fixture creation."))
  (princ))
(princ)
