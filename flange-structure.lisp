(defconstant *pi* 3.141592653589793d0)
(defconstant *2pi* 6.283185307179586d0)
(defparameter center '(750.0d0 250.0d0 0.0d0))
(defparameter holes-count 12)
(defparameter Z0 '(0.0 0.0 0.0))
(defparameter ZSafe '(0.0 0.0 50.0d0))
(defparameter Zstart '(0.0 0.0 16.0d0))
(defparameter R0 (/ 445.00d0 2.0)) ;; ακτίνα κύκλου οπών
(defparameter f-drill-up 500.00) ;;προωση down
(defparameter f-drill-down 500.00) ;;προωση up
(defparameter d1 4.0d0) ;;; διάμετρος εργαλείου
(defparameter r1 (- 11 (/ d1 2.0))) ;;ακτίνα οπών
(defparameter fcut 500.0) ;;; προωση κοπής κύκλου
(defparameter file_1 "/home/quill/linuxcnc/lisp/f355/helical-D445-d22-t4.0-750x250.cnc")
(defparameter *stream* *STANDARD-OUTPUT*)


(defstruct cut-data
  (center '(750.0d0 250.0d0 0.0d0))
  (holes-count 12)
  (Z0 '(0.0 0.0 0.0))
  (ZSafe '(0.0 0.0 50.0d0))
  (Zstart '(0.0 0.0 16.0d0))
  (R0 (/ 445.00d0 2.0)) ;; ακτίνα κύκλου οπών
  (f+ 500.00) ;;προωση up
  (f- 500.00) ;;προωση down
  (tool-diameter 3.175d0) ;;; διάμετρος εργαλείου
  (hole-diameter 9.5d0) ;;ακτίνα οπών
  (points-array '())
  (hole-cut-radius 0.0d0  ) ;;; ακτίνα κοπής
  (fxy 500.0) ;;; προωση κοπής
  (fxy1 500.0) ;;; προωση κοπής 1
  (frapid 1500.0) ;; ταχεία πρόωση
  (file_1 "/home/quill/linuxcnc/lisp/f355/helical-D445-d22-t4.0-750x250.cnc")
  (*stream* *STANDARD-OUTPUT*)
  )

(setq flange-1 (make-cut-data))

(setf (cut-data-center flange-1) '(0.0d0 0.0d0 0.0d0)) ; κέντρο κύκλου
(setf (cut-data-R0 flange-1) (/ 445.0 2.0)) ; ακτίνα οπών
(setf (cut-data-tool-diameter flange-1) 4.0d0) ; διάμετρος εργαλείου
(setf (cut-data-hole-diameter flange-1) 22.0)  ; ακτίνα κύκλου
(setf (cut-data-hole-cut-radius flange-1) (/ (- 22 4.0)2.0d0))  ; ακτίνα κύκλου
(setf (cut-data-file_1 flange-1) "/home/quill/linuxcnc/lisp/f355/helical-D445-d22-t4.0-750x250.cnc")

(defun set-file-name (flange pathname)
  (setf (cut-data-file_1 flange) pathname)
  )

(set-file-name flange-1 "path2")

(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

(defun add-vector-point (point-1 point-2)
  (mapcar #'+ point-1 point-2))

(defun s+ (s1 s2)
  (concatenate 'string s1 s2))

(defun rad-to-deg (a)
  (* (/ 180.0 *pi*) a))

(defun deg-to-rad (a)
  (/ a (/ 180.0 *pi*)))

(defun holes-center (flange)
  (let ((center (cut-data-center flange))
	(R0 (cut-data-R0 flange))
	(holes-count (cut-data-holes-count flange))
	)
  (loop for i from 0 to (- holes-count 1.0)
	collect (let* ((angle (* i 2.0d0 (/ *pi* holes-count)))
		      (X (+ (x-of center) (* R0 (cos angle))))
		      (Y (+ (y-of center) (* R0 (sin angle))))
		      (Z (z-of center)))
	(list  X Y Z)
		  ))))

;;;
(holes-center flange-1)

(defun goto (point str)
  (format str "G0 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T ~%" (x-of point) (y-of point) (z-of point)))

(defun linear-move (point f str)
  (format str "G1 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T F~D ~%" (x-of point) (y-of point) (z-of point) f))

(defun clockwise-move-ij (point i j f str)
  (format str "G2 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  I~8,3F ~35T J~8,3F ~45T F~D ~%" (x-of point) (y-of point) (z-of point) i j f))

(defun clockwise-move-R (point r f str)
  (format str "G2 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(drill-circle '(0 0 100.0) (/(- 9.5 3.175)2.0) 500.0 *STANDARD-OUTPUT*)

(defun drill-circle (point r1 ri fcut str)
  ;;;
  (let ((xi (x-of point))
	(yi (y-of point))
	(zi (z-of point)))
    ;;(linear-move (list (+ xi ri) yi zi) fcut str)
   (goto (list (+ xi ri) yi zi) str)
   (clockwise-move-R (list (- xi r1 ) (+ yi ) zi) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) zi) ri fcut str)
   (clockwise-move-R (list (- xi r1 ) (+ yi ) (* zi 0.75)) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) (* zi 0.75)) ri fcut str)

   (clockwise-move-R (list (- xi r1 ) (+ yi ) (* zi 0.5)) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) (* zi 0.5)) ri fcut str)

   (clockwise-move-R (list (- xi r1 ) (+ yi ) (* zi 0.25)) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) (* zi 0.25)) ri fcut str)
   
   (clockwise-move-R (list (- xi r1 ) (+ yi ) 0.0d0) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) 0.0d0) ri fcut str)
   (clockwise-move-R (list (- xi r1 ) (+ yi ) 0.0d0) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) (* zi 0.25)) ri fcut str)
   (clockwise-move-R (list (- xi r1 ) (+ yi ) (* zi 0.5)) ri fcut str)
   (clockwise-move-R (list (+ xi r1 ) (+ yi ) (* zi 0.75)) ri fcut str)
   (clockwise-move-R (list (- xi r1 ) (+ yi ) zi) ri fcut str)
    (clockwise-move-R (list (+ xi r1 ) (+ yi ) zi) ri fcut str)
    (format t "~%" str)
					;    (goto (list (+ xi ri) yi zi) str)
					;    (goto (list (+ xi ri) yi zi) str)

   ))




(defun drilling-cycle (flange str)
  (let ((holes-centers (hole-center flange))
	(Zsafe (cut-data-ZSafe flange))
	(Zstart (cut-data-Zstart flange))
	(r1 (cut-data-r1 flange)))
    (loop for i in holes-centers
	  do (progn
	       (let ((nu (add-vector-point i Zsafe))
		     (ns (add-vector-point i Zstart))
		     (nd (add-vector-point i '(0 0 0.0))))
		 
		 (goto nu str)
		 (drill-circle ns r1 fcut str)
		 (goto nu str)
		 (format str "~%" )
		 ))
	  )))

(drilling-cycle flange-1 *STANDARD-OUTPUT*)

(with-open-file (stream file_1  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (drilling-cycle center R0 holes-count stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)



(defparameter center '(250.0d0 250.0d0 0.0d0))
(defparameter holes-count 12)
(defparameter Z0 '(0.0 0.0 0.0))
(defparameter ZSafe '(0.0 0.0 50.0d0))
(defparameter Zstart '(0.0 0.0 16.0d0))
(defparameter R0 (/ 445.00d0 2.0)) ;; ακτίνα κύκλου οπών
(defparameter f-drill-up 500.00) ;;προωση down
(defparameter f-drill-down 500.00) ;;προωση up
(defparameter d1 4.0d0) ;;; διάμετρος εργαλείου
(defparameter r1 (- 11 (/ d1 2.0))) ;;ακτίνα οπών
(defparameter fcut 500.0) ;;; προωση κοπής κύκλου
(defparameter file_1 "/home/quill/linuxcnc/lisp/f355/helical-D445-d22-t4.0-250x250.cnc")
(defparameter *stream* *STANDARD-OUTPUT*)

(with-open-file (stream file_1  :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (drilling-cycle center R0 holes-count stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)
