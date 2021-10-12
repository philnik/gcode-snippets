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
  (f-drill-up 500.00) ;;προωση down
  (f-drill-down 500.00) ;;προωση up
  (d1 4.0d0) ;;; διάμετρος εργαλείου
  (r1 0.0) ;;ακτίνα οπών
  (fcut 500.0) ;;; προωση κοπής κύκλου
  (file_1 "/home/quill/linuxcnc/lisp/f355/helical-D445-d22-t4.0-750x250.cnc")
  (*stream* *STANDARD-OUTPUT*)
  )

(setq cond-1  (make-cut-data))

(setf (cut-data-r1 cond-1) 20.0)
      
(defstruct cut-data
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



(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

(defun holes-center (center R0 holes-count)
  (loop for i from 0 to (- holes-count 1.0)
	collect (let* ((angle (* i 2.0d0 (/ 3.141592653 holes-count)))
		      (X (+ (x-of center) (* R0 (cos angle))))
		      (Y (+ (y-of center) (* R0 (sin angle))))
		      (Z (z-of center)))
	(list  X Y Z)
	)))

(defun goto (point str)
  (format str "G0 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T ~%" (x-of point) (y-of point) (z-of point)))

(defun linear-move (point f str)
  (format str "G1 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T F~D ~%" (x-of point) (y-of point) (z-of point) f))

(defun clockwise-move-ij (point i j f str)
  (format str "G2 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  I~8,3F ~35T J~8,3F ~45T F~D ~%" (x-of point) (y-of point) (z-of point) i j f))

(defun clockwise-move-R (point r f str)
  (format str "G2 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(defun add-vector-point (point-1 point-2)
(mapcar #'+ point-1 point-2))

(defun drill-circle (point ri fcut str)
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
					;    (goto (list (+ xi ri) yi zi) str)
					;    (goto (list (+ xi ri) yi zi) str)

   ))

(defun drilling-cycle (center R0 holes-count str)
(loop for i in (holes-center center R0 holes-count)
      do (progn
	   (let ((nu (add-vector-point i Zsafe))
		 (ns (add-vector-point i Zstart))
		 (nd (add-vector-point i '(0 0 0.0))))
		 
;	     (linear-move nd f-drill-down str)
;	     (linear-move nu f-drill-up str)
					;	     (linear-move nu f-drill-up str)
	     (goto nu str)
	     (drill-circle ns r1 fcut str)
	     (goto nu str)
;	     (linear-move nu f-drill-up str)
	     (format str "~%" )
			   ))
      ))

(drilling-cycle center R0 holes-count *STANDARD-OUTPUT*)

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
