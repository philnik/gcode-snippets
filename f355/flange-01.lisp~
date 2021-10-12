



(defparameter center '(00.0d0 00.0d0 0.0d0))

(defparameter holes-count 4)

(defparameter Z0 '(0.0 0.0 0.0))
(defparameter ZSafe '(0.0 0.0 50.0d0))
(defparameter Zstart '(0.0 0.0 16.0d0))

(defparameter R0 100) ;; ακτίνα κύκλου οπών

(defparameter f-drill-up 500.00) ;;προωση down
(defparameter f-drill-down 500.00) ;;προωση up

(defparameter d1 1.0d0) ;;; διάμετρος εργαλείου
(defparameter r1 (- 10.0 (/ d1 2.0))) ;;ακτίνα οπών

(defparameter fcut 500.0) ;;; προωση κοπής κύκλου



(defparameter *stream* *STANDARD-OUTPUT*)


(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

(setq bridges-angle (/ bridges-length Rout))


(defun holes-center (center R0 holes-count)
  (loop for i from 0 to (- holes-count 1.0)
	collect (let* ((angle (* i 2.0d0 (/ 3.141592653 holes-count)))
		      (X (+ (x-of center) (* R0 (cos angle))))
		      (Y (+ (y-of center) (* R0 (sin angle))))
		      (Z (z-of center)))
	(list  X Y Z)
	)))
  
;(holes-center (list 0 0 0) 10 8)


(defparameter current-point '(0.0 0.0 0.0))

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
   (goto (list (+ xi ri) yi zi) str)
   )
  )




(defun drilling-cycle (center R0 holes-count str)
(loop for i in (holes-center center R0 holes-count)
      do (progn
	   (let ((nu (add-vector-point i Zsafe))
		 (ns (add-vector-point i Zstart))
		 (nd (add-vector-point i '(0 0 0.0))))
		 
	     (goto nu str)
	     (linear-move nd f-drill-down str)
	     (linear-move nu f-drill-up str)
	     (linear-move ns f-drill-up str)
	     (drill-circle ns r1 fcut str)
			   ))
      )
)

(drilling-cycle center R0 holes-count *STANDARD-OUTPUT*)


(with-open-file (stream "/home/quill/linuxcnc/nc_files/circles.ngc" :direction :output :if-exists :overwrite :if-does-not-exist :create)

  (drilling-cycle center R0 holes-count stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)

  


