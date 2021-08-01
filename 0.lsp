
(defconstant *pi* 3.141592653589793d0)
(defconstant *2pi* 6.283185307179586d0)
(defparameter center '(100.0d0 100.0d0 100.0d0))
(defparameter holes-count 12)
(defparameter bridges-length 4.0) ;; μήκος bridges
(defparameter Rout 140.00d0) ;;εξωτερικός κύκλος
(defparameter no-bridges 8)
(defparameter Z0 '(0.0 0.0 0.0))
(defparameter ZSafe '(0.0 0.0 25.0d0))
(defparameter Zstart '(0.0 0.0 12.0d0))
(defparameter R0 100) ;; ακτίνα κύκλου οπών
(defparameter Rout 150) ;; ακτίνα εξωτερικού κύκλου
(defparameter Rslot 50) ;; ακτίνα εσωτερικού κύκλου(πατουρα)
(defparameter Rin 45) ;; ακτίνα εσωτερικού κύκλου
(defparameter h 5) ;; Υψος πατούρας
(defparameter fz+ 500.00) ;;προωση down
(defparameter fz- 500.00) ;;προωση up
(defparameter hole-diameter 10.0) ;;διάμετρος οπών
(defparameter fxy 1500.0) ;;; ταχεία πρόωση
(defparameter frapid 3000.0) ;;; προωση κοπής κύκλου
(defparameter tool-diameter-3175 3.175d0) ;;; διάμετρος εργαλείου κοπής
(defparameter tool-diameter-8 8d0) ;;; διάμετρος εργαλείου
(defparameter *stream* *STANDARD-OUTPUT*)
(defparameter *pathdir* "/home/quill/linuxcnc/lisp/")



(defun s+ (s1 s2)
  (concatenate 'string s1 s2))

(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

(defun goto (point str)
  (format str "G0 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F ~35T ~%" (x-of point) (y-of point) (z-of point)))

(defun linear-move (point f str)
  (format str "G1 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F ~35T F~D ~%" (x-of point) (y-of point) (z-of point) f))

(defun cw-ij (point i j f str)
  (format str "G2 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F  I~8,4F ~35T J~8,4F ~45T F~D ~%" (x-of point) (y-of point) (z-of point) i j f))

(defun cw-R (point r f str)
  (format str "G2 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F  R~8,4F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(defun ccw-ij (point i j f str)
  (format str "G3 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F  I~8,4F ~35T J~8,4F ~45T F~D ~%" (x-of point) (y-of point) (z-of point) i j f))

(defun ccw-R (point r   &optional &rest keys &key (f fxy) (str nil))
  (format str "G3 ~4T X~8,4F ~15T Y~8,4F ~25T Z~8,4F  R~8,4F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(setf fxy 1000.0)
(let ((fxy 100.0))
  (ccw-R '(0 0 0) 1)





(defun point+ (point-1 point-2)
(mapcar #'+ point-1 point-2))

(defun point* (point sc)
(mapcar #'* (list sc sc sc) point))

(defun rad-to-deg (a)
  (* (/ 180.0 *pi*) a))

(defun deg-to-rad (a)
  (/ a (/ 180.0 *pi*)))

(defun arc-length-to-rad (arc-length radius)
  (/ arc-length radius 1.0d0))

(defun polar-to-rect-rad (center radius angle)
  "converts polar to rectangular with center,radius and angle"
  "rad angle version"
  (let* ((xi (x-of center))
	 (yi (y-of center))
	 (zi (z-of center))
	 )
    (list (+ xi (* radius (cos angle))) (+ yi (* radius (sin angle))) zi)
    )
  )

(defun polar-to-rect-deg (center radius angle)
  "converts polar to rectangular with center,radius and angle"
  "deg angle version"
  (polar-to-rect-rad center radius (deg-to-rad angle))
  )

(defun test-polar-to-rect ()
(loop for i in '(0 45 90 135 180 225  270 315 360)
      do (let* ((no (polar-to-rect-deg '(1.0d0 0.0d0 0.0d0) 1 i))
		(xi (x-of no))
		(yi (y-of no))
		)
	   (format t "------------------------")
	   (format t "angle:~2,3F     x:~2,3F          y:~2,3F~%" i xi yi)
	   ))
	   )

(defun cut-circle-radius (point cut-radius fxy str)
  (let* ((xi (x-of point))
	 (yi (y-of point))
	 (zi (z-of point))

	 (-NE- (polar-to-rect-deg point cut-radius 45))
	 (-N-  (polar-to-rect-deg point cut-radius 90))
	 (-W-  (polar-to-rect-deg point cut-radius 180))
	 (-SW- (polar-to-rect-deg point cut-radius 225))
	 (-S-  (polar-to-rect-deg point cut-radius 270))
	 (-SE- (polar-to-rect-deg point cut-radius 315))
	 (-E-  (polar-to-rect-deg point cut-radius 0))
	 )

	 (goto (point+ -N- Zsafe) str)
	 (goto (point+ -N- Zstart) str)
	 
	 (cw-R (point+ (point* Zstart 0.50) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -E-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -N-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 1.00) -SE-) cut-radius  (* 2 fxy) str)
	 (goto (point+ -SE- Zsafe) str)
	 )
  )


(cut-circle-radius '(0 0 0)  50.0 00.0 *STANDARD-OUTPUT*)



(defun bridge-angles (bridge-angle no-bridges)
  (let ((step-angle (- (/ *2pi* no-bridges) bridge-angle)))
    (loop for i from 0 to (- no-bridges 1)
	  collect
	  (let
	      (
	       (j1 (+ (* i (+ bridge-angle step-angle)) step-angle))
	       (j2 (+ (* i (+ bridge-angle step-angle)) step-angle (/ bridge-angle 2.0)))
	       (j3 (+ (* (+ i 1) (+ bridge-angle step-angle))))
	       )
	    (list j1 j2 j3)
	    )
	  )))


(defun print-bridge-angles (bridge-angle no-bridges)
  (let ((list-angles (bridge-angles bridge-angle no-bridges)))
    (loop for i from 0 to (- no-bridges 1)
	  do (let ((angles (nth i list-angles)))
	       (format t "i~d=~8,4F~%" i          (rad-to-deg (nth 0 angles)))
	       (format t "j~d=~8,4F~%" i   (rad-to-deg (nth 1 angles)))
	       (format t "k~d=~8,4F~%" i   (rad-to-deg (nth 2 angles)))
	       ))))

;(format t "-----------------------")
;(bridge-angles (arc-length-to-rad 4.0d0 100.0) 5)
;(print-bridge-angles (arc-length-to-rad 4.0d0 100.0) 5)

(defun outer-circle (point diameter tool-diameter fxy str)
  (let 	 ((radius (/ (+ diameter tool-diameter) 2.0d0)))
    (cut-circle-radius point radius fxy str))) 

(defun inner-circle (point diameter tool-diameter fxy str)
  (let 	 ((radius (/ (- diameter tool-diameter) 2.0d0)))
    (cut-circle-radius point radius fxy str)))

(defun holes-center (center R0 holes-count)
  (loop for i from 0 to (- holes-count 1.0)
	collect (let* ((angle (* i 2.0d0 (/ 3.141592653 holes-count)))
		      (X (+ (x-of center) (* R0 (cos angle))))
		      (Y (+ (y-of center) (* R0 (sin angle))))
		      (Z (z-of center)))
	(list  X Y Z)
	)))
  
(defun helical-drill (point hole-diameter tool-diameter fxy str)
  (let* ((xi (x-of point))
	 (yi (y-of point))
	 (zi (z-of point))

	 (cut-radius (/ (- hole-diameter tool-diameter)2.0d0))

	 (-NE- (polar-to-rect-deg point cut-radius 45))
	 (-N-  (polar-to-rect-deg point cut-radius 90))
	 (-W-  (polar-to-rect-deg point cut-radius 180))
	 (-SW- (polar-to-rect-deg point cut-radius 225))
	 (-S-  (polar-to-rect-deg point cut-radius 270))
	 (-SE- (polar-to-rect-deg point cut-radius 315))
	 (-E-  (polar-to-rect-deg point cut-radius 0))
	 )
    
	 
	 (goto (point+ -N- Zsafe) str)
	 (goto (point+ -N- Zstart) str)
	 
	 (cw-R (point+ (point* Zstart 0.90) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.80) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.70) -E-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.60) -N-) cut-radius  fxy str)

	 (cw-R (point+ (point* Zstart 0.50) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.40) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.30) -E-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.20) -N-) cut-radius  fxy str)
	 	 
	 (cw-R (point+ (point* Zstart 0.00) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -E-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -N-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.00) -W-) cut-radius  fxy str)

	 (cw-R (point+ (point* Zstart 0.25) -S-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.50) -E-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 0.75) -N-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 1.00) -W-) cut-radius  fxy str)
	 (cw-R (point+ (point* Zstart 1.25) -S-) cut-radius  fxy str)
	 
	 (goto (point+ -N- Zsafe) str)
	 )
  )


(defun hellical-drill-cycle (center hole-diameter tool-diameter R0 holes-count fxy str)
  (loop for i in (holes-center center R0 holes-count)
	do (progn
	     (let ((nu (point+ i Zsafe))
		   (ns (point+ i Zstart))
		   (nd (point+ i '(0 0 0.0))))
		 
	       (goto nu str)
	       (helical-drill center hole-diameter tool-diameter fxy str)
			   ))
      )
  )


(defun drill-cycle (center R0 holes-count str)
(loop for i in (holes-center center R0 holes-count)
      do (progn
	   (let ((nu (point+ i Zsafe))
		 (ns (point+ i Zstart))
		 (nd (point+ i '(0 0 0.0))))
		 
	     (goto nu str)
	     (linear-move nd fz- str)
	     (linear-move nu fz+ str)
	     (linear-move ns fz+ str)
;;	     (drill-circle ns r1 fxy str)
			   ))
      )
)

(format t "(circles-8.0)~%")
(helical-drill center 9.5d0 3.175d0 600 *STANDARD-OUTPUT*)

(hellical-drill-cycle center 10.0 3.175 100.0 8.0 500 *STANDARD-OUTPUT*)

(format t "(circles-3.175)~%")
(drill-cycle (list 1000.0 1000.0 100.0)  200.0 12.0 *STANDARD-OUTPUT*)
(format t "(drilling D10)~%")

;(outer-circle (list 100.0 100.0 100.0)  50.0 12.0 *STANDARD-OUTPUT*)
					;(drilling-cycle-2 center R0 holes-count *STANDARD-OUTPUT*)
;(outer-circle center  50.0 12.0 500.0 *STANDARD-OUTPUT*)
;(inner-circle center  50.0 12.0 500.0 *STANDARD-OUTPUT*)

;; ;; (defmacro write-code-to-file (code file)
;; ;;   `(
;; ;;    (with-open-file (stream ,file :direction :output :if-exists :overwrite)
;; ;;      (format stream ,code)
;; ;;      )
;; ;;    )
;; ;;   )

;; (write-code-to-file (helical-program center R0 holes-count) (s+ *pathdir*  "helical.ngc"))

(defun helical-program (center R0 holes-count)
  (with-output-to-string (stream fstr)
    (drill-cycle center R0 holes-count stream)
    (format stream "~%")
    (format stream "M30~%")
    (format stream "~%")
    (format stream "%")
    (format stream "%")
    )
  fstr
  )


 (setq fstr (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))

(setq fstr "")
(with-output-to-string (s fstr)
    (format s "here's some output")
    (input-stream-p s))
 fstr

   
   (with-open-file (stream "/home/quill/linuxcnc/nc_files/circles.ngc" :direction :output :if-exists :overwrite)

  (drill-cycle center R0 holes-count stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)


(with-open-file (stream "/home/quill/linuxcnc/nc_files/helical-drill.ngc" :direction :output :if-exists :overwrite :if-does-not-exist :create)
  (hellical-drill-cycle center 30.0 3.175 100.0 12.0 500 stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)


;; currently works
(hellical-drill-cycle '(0 0 0) 10.0 4 100.0 12.0 1700 *STANDARD-OUTPUT*)
;;;

(with-open-file (stream (s+ *pathdir*  "outer-circle.ngc") :direction :output :if-exists :overwrite))

  (outer-circle (list 100.0 100.0 100.0)  50.0 3.0 500.0 stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)


(with-open-file (stream  (s+ *pathdir* "inner-circle.ngc") :direction :output :if-exists :overwrite)

  (inner-circle (list 100.0 100.0 100.0)  50.0 3.0 500.0 stream)
  (format stream "~%")
  (format stream "M30~%")
  (format stream "~%")
  (format stream "%")
  (format stream "%")
)


;;check output as a list

(defmacro c (center radius)
  `(list 'command "circle" ,center "r" ,radius)
  )

(c '(list 0 0) 1)

(format t "~S~%" '(command "circle" (list 0 0) "R" 3))

(format t "~S~%" (c '(list 0 0) 1))

  


