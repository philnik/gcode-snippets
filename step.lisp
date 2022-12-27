;;;; I find CLOSER-MOP functions easier to use than
;;;; implementation-specific functions
(quicklisp:quickload :closer-mop)


(defparameter *crlf* (format nil "~C~C" #\return #\linefeed))

(defconstant *windows* t)
(defconstant *pi* 3.141592653589793d0)
(defconstant *2pi* 6.283185307179586d0)

(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

(defun rad-to-deg (a)
  "rad-to-deg converts radians to degrees
`a' angle in radians
"
  (* (/ 180.0d0 *pi*) a))

(defun deg-to-rad (a)
  "deg-to-rad converts degrees to radians
`a' angle in degrees
"
  (/ a (/ 180.0d0 *pi*)))

(defun atan2 (y x)
"atan2 returns tangent
`y' y coordinate
`x' x coordinate
"
  (cond ((> x 0.0d0) (atan (/ y x)))
	((and (< x 0.0d0) (>= y 0.0d0)) (+ (atan (/ y x)) *pi*))
	((and (< x 0.0d0) (< y 0.0d0))  (- (atan (/ y x)) *pi*))
	((and (= x 0.0d0) (> y 0.0d0))  (* 0.5 *pi*))
	((and (= x 0.0d0) (< y 0.0d0))  (* -0.5 *pi*))
	(t "error")
	))

;; #+name: vector-functions
(defun no-of-points-from-arc-radius (steps radius)
"no-of-points-from-arc-radius: get number of points to divide a cricle at step arc length
`steps' arc length  
`radius' radius circle
"
  (let* ((divide (/ (* *2pi* radius) steps))
         )
;;turn to integer and find the closest to make 4-quarters
    (* (round (/ divide 1.0))1))
  )


(defun length-vector (v)
  "vector length
`v' ((p1x p1y) (p2x p2y))
"
  (let ((cc 
	(destructuring-bind (p1 p2)
	    v
	  (destructuring-bind (p1x p1y)
	      p1
	    (destructuring-bind (p2x p2y)
		p2
	      (sqrt (+ (expt (- p2x p1x) 2.0d0) (expt (- p2y p1y) 2.0d0)))
	      )))
	))
    cc
    ))
  
(defun normal-vector-2 (v)
  "normal-vector-2 normalize vector keeping start point
`v' vector"
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (angle
             (if (= p1x p2x)
                 (if (> p2y p1y)
                     (* *pi* 0.5d0)
                     (* *pi* -0.5d0))
                 (atan2 (- p2y p1y) (- p2x p1x))
                 )))
      (list (list p1x p1y) (list (+ p1x (cos angle)) (+ p1y (sin angle))))
      ))

(defun normal-vector (v)
  "normal-vector normalize vector and transforms to zero
`v' vector"
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
	   (l (sqrt (+ (expt (- p2y p1y)2.0d0)(expt (- p2x p1x)2.0d0))))
	   (vec
             (if (= p1x p2x)
		 (if (> p2y p1y)
                     (list 1.0d0 0.0)
		     (list -1.0d0 0.0d0))
		 (list (/ (- p2x p1x) l) (/ (- p2y p1y)l)))))
      (list '(0.0d0 0.0d0) vec)))

(defun angle-vector (v)
  (let* ((p1 (nth 0 v))
         (p2 (nth 1 v))
         (p1x (x-of p1))
         (p1y (y-of p1))
         (p2x (x-of p2))
         (p2y (y-of p2)))
    (if (= p1x p2x)
        (if (> p2y p1y)
            (* *pi* 0.5d0)
            (* *pi* -0.5d0))
        (atan2 (- p2y p1y) (- p2x p1x))))
  )

(defun p+ (p1 p2)
  (let* ((p1x (x-of p1))
         (p1y (y-of p1))
         (p2x (x-of p2))
         (p2y (y-of p2))
         )
    (list (+ p1x p2x) (+ p1y p2y))
    ))

(defun scale-point (p scale-factor)
  (let* ((px (x-of p))
         (py (y-of p))
         )
    (list (* scale-factor px) (* scale-factor py))
    ))

(defun middle-point (point-min point-max)
  (let ((x (* 0.5 (+ (x-of point-min) (x-of point-max))))
        (y (* 0.5 (+ (y-of point-min) (y-of point-max)))))
    (list x y)))

(defun middle-vector (v)
  "middle-vector: finds the middle point of start & end point of a vector
`v' vector ( array of 2 points)
"
  (middle-point (nth 0 v) (nth 1 v)))


(defun vector-scale (v scale-factor)
"vector-scale scales a vector, holds start point scales length by scale-factor for new end-point
`v' vector
`scale-factor' length multiplier"
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (length (length-vector v))
           (angle (angle-vector v)))
      (list p1 (list (+ p1x (* scale-factor length (cos angle))) (+ p1y (* scale-factor length (sin angle)))))
      ))

(defun vector-rotate (v b-angle)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (length (length-vector v))
           (angle (angle-vector v))
           (new-angle (+ angle b-angle)))
      (list p1 (list (+ p1x (* length (cos new-angle))) (+ p1y (* length (sin new-angle)))))
      ))

(defun vector-rotate-deg (v b-angle-deg)
  (vector-rotate v (* (/ *pi* 180.0) b-angle-deg))
      )

(defun vector-transform-to-zero (v)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (tr-point (list (- p2x p1x) (- p2y p1y))))
      (list (list 0.0d0 0.0d0) tr-point)
      ))

(defun vector-transform-by-vector (v tr-vector)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (zero-vector (vector-transform-to-zero tr-vector))
	   (tr-point (cadr zero-vector)))
      (list (p+ p1 tr-point) (p+ p2 tr-point))
      ))

(defun vector-offset (v distance)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
	   (vrotate (vector-rotate v (* *pi* 0.5)))
	   (normal-vrotate (normal-vector vrotate))
	   (dvector (vector-scale normal-vrotate distance))
	   )
      (progn
	(vector-transform-by-vector v dvector)
	)))

(defun vector-reverse (v)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v)))
      (list p2 p1)
      ))

;; [[file:step.org::streams][streams]]
(defun prologue (spindle output-stream)
  (format output-stream
	  (concatenate 'string
		       "(prologue)" *crlf*
		       (format nil "M03 S~d" spindle) *crlf*
		       "(end of prologue)" *crlf*)
	  ))


(defun epilogue (output-stream)
  (format output-stream
	  (concatenate 'string
		       *crlf*
		       "(epilogue)" *crlf*
		       "M05" *crlf*
		       "M30" *crlf*
		       "(end of program)" *crlf*
		       "%%" *crlf*)
	  ))

;; [move-functions]
(defun goto (point str)
  (let ((command-line (format nil "G00 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T"
			      (x-of point)
			      (y-of point)
			      (z-of point)))
	)
    (format str (concatenate 'string command-line *crlf*))
    ))

(defmacro add-feed (f)
  `(if ,f
       (format nil " F~D" f)))


(defun linear-move-command (point &optional f)
  (concatenate 'string
	       (format nil "G01 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T"
		       (x-of point)
		       (y-of point)
		       (z-of point)
		       )
	       (add-feed f)
	       (if *windows*
		   *crlf*
		   (format nil "~%")
	       )))

;;(linear-move-command '(0 0 0) 3)

(defun linear-move (point f str)
  (format str
	  (linear-move-command point f)
	  ))

(defun clockwise-move-ij (point i j f str)
  (let ((command-line
	 (format nil "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  I~8,3F ~35T J~8,3F ~45T F~D"
			      (x-of point)
			      (y-of point)
			      (z-of point) i j f)))
(format str (concatenate 'string command-line *crlf*))))

(defun clockwise-move-R (point r f str)
  (let ((command-line
	 (format nil "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D"
		 (x-of point)
		 (y-of point)
		 (z-of point) r f)))
    (format str (concatenate 'string command-line *crlf*))
    ))

(defun cw-move-R (point r f str)
  "Clockwise interpolation G02 gcode string
`point' endpoint of the arc
`r' radius of the arc
`f' feedrate of the move
`str' output-stream"
  (let ((command-line
	 (format nil "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D"
		 (x-of point)
		 (y-of point)
		 (z-of point)
		 r f))
	)
    (format str (concatenate 'string command-line *crlf*))
    ))

(defun ccw-move-R (point r f str)
"Counter clockwise interpolation G03 gcode string
`point' endpoint of the arc
`r' radius of the arc
`f' feedrate of the move
`str' output-stream"
(let ((command-line
       (format nil "G03 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D"
	       (x-of point)
	       (y-of point)
	       (z-of point) r f))
      )
  (format str (concatenate 'string command-line *crlf*))
  ))

(defun counter-clockwise-move-R (point r f str)
  "G03 interpolations calls ccw-move-R"
  (ccw-move-R point r f str))
;; move-functions ends here

;; [step-function]
(defun z-step-list (zstart zend step)
  "step-over splits a z-height on steps
`zstart' start z
`zend' bottom z
`step' z step"
    (let* ((dz (- zstart zend))
           (no-steps(+ 0 (floor (/ dz step))))
           (result '())
           )
      (push zstart result)
      (dotimes (n no-steps)
        (push (- (car result) step) result)
        )
      (if (/= zend (car result))
          (push zend result))
       result)
  )

(defun helical-z-list (zsafe zstart zend zstep)
  "helical-z-list creates the list of heights for helical drilling
`zsafe' top z
`start' helical start here
`end' helical ends here
`zstep' z step each helical"
  (let ((steps-down (z-step-list zend zstart (- 0.0 zstep)))
	(steps-up (z-step-list zstart zend (* 2.0 zstep))))
    (append (list zsafe) steps-down steps-up (list zsafe))))

(defun helical-z-list-retract (zsafe zup zdown zretract zstep+ zstep-)
  "helical-z-list creates the list of heights for helical drilling
`zsafe' top z
`zup' helical start here
`zdown' helical ends here
`zretract' helical return height
`zstep+' z step down
`zstep-' z step up
"
  (cond ((< zup zdown) (cerror "low zup"))
	  ((< zretract zdown) (cerror "low zretract"))
	  )
  (append
     (setq b
	   (loop for i from zup downto zdown by zstep+
		 collect  i)
	   )
     
     (if (not (= (car (last b)) zdown))
	 (list zdown))
     
     (setq c
	   (loop for i from zdown upto zretract by zstep-
		 collect  i))
     
     (if (not (= (car (last c)) zretract))
	 (list zretract))
     ))

(helical-z-list-retract 20 10 0 0 1 1)


;; Divide a circle on points
;; [divide-circle]
(defun polar-to-rect (center radius angle)
    (list
     (+ (x-of center) (* radius (cos angle)))
     (+ (y-of center) (* radius (sin angle)))
     ))

(defun divide-circle (center radius no-of-points)
  "divide circles to points, starts from 0deg moving clockwise
`center' center of the circle
`radius' radius of the points
`no-of-points' number of points"
      (let ((angle (/ *2pi* no-of-points))
            (point-array '()))

        (dotimes (n  no-of-points)
          (push (polar-to-rect center radius (* n angle)) point-array)
          )
      (reverse point-array)
        ))


(defclass arc ()
  ((center
    :initarg :center
    :accessor center)
   (start-angle
    :initarg :start-angle
    :accessor start-angle)
   (end-angle
    :initarg :end-angle
    :accessor end-angle)
   (direction
    :initarg :direction
    :accessor direction)
   (radius
    :initarg :radius
    :accessor radius)
   (no-of-points
    :initarg :no-of-points
    :accessor no-of-points)
))


(defun included-angle (start-angle end-angle direction &optional (fca *2pi*))
  (case direction
    ('ccw
     "ccw"
     (mod (+ fca (- end-angle start-angle)) fca)
     )
    ('cw
     "cw"
     (mod (+ fca (- start-angle end-angle)) fca)
     )
    ))


(defun divide-arc (center radius start-angle end-angle direction  no-of-points)
  "divide arc to points, starts from 0deg moving clockwise
`center' center of the circle
`start-angle'
`end-angle'
`direction' 'cw or 'ccw
`radius' radius of the points
`no-of-points' number of points"
  (if (< no-of-points 2)
      (error "Divide arc:at least 2 points")
      )
  (let* ((included-angle (included-angle start-angle end-angle direction))
	 (angle (/ included-angle (- no-of-points 1)))
            (point-array '()))

        (dotimes (n  no-of-points)
          (push (polar-to-rect center radius (+ start-angle (* n angle))) point-array)
          )
    
      (reverse point-array)
        ))

;; divide-circle ends here

;;;divide-line
(defun divide-line (vector step-length)
  "divides a line to points
`vector' descibes start-point->end-point of line
`no-of-points' number of points"
  (let* ((l (length-vector vector))
	 (no-of-points ( round (/ l step-length)))
	 (step-round (/ l no-of-points))
	 (nvector (normal-vector-2 vector))
         (point-array '()))

    (dotimes (n  (+ 1 no-of-points))
      (push (vector-scale nvector (* n step-round)) point-array)
          )
      (reverse point-array)
        ))

(defun line-point-couples (v trochoidal-width step-length)
  "line-point-couples returns couples on a straight line 
`v' vector  
`trochoidal-width' width of the trochoidal
`step-length' couple distance"
  (let* ((dv+ (vector-offset v (* trochoidal-width 0.5d0)))
	 (dv- (vector-offset v (* trochoidal-width -0.5d0)))
	 (dl+  (divide-line dv+ step-length))
	 (dl-  (divide-line dv- step-length))
	 (couples '()))
    (dotimes (n (length dl+))
      (let ((i (cadr (nth n dl+)))
	    (j (cadr (nth n dl-))))
	(push (list (list (car i) (cadr i)) (list (car j) (cadr j))) couples)
	))
    (reverse couples)))

;; [helical-drill-at-point]
(defun helical-drill (point zsafe zstart zend zstep radius f output-stream)
  "helical drill make an helical drill at point
`point' point to helical drill
`zsafe' top z point
`zstart' start helica at this z point
`zend' end helica at this z point
`zstep' step at this point 
`radius' helica radius
`f' feedrate
`output-stream' where to write"
  (let ((xi (x-of point))
        (yi (y-of point))
	(z-list (helical-z-list zsafe zstart zend zstep))
	)

    (format output-stream
	    (concatenate 'string
			 *crlf*
			 (format nil "(helical drilling point: X~8,3F Y~8,3F)" (x-of point) (y-of point))
			 *crlf*))

    (goto (list xi yi (pop z-list)) output-stream)
    (goto (list (+ xi radius ) yi (pop z-list)) output-stream)

    (loop while (cddr z-list)
          do (progn
               (clockwise-move-R (list (- xi radius ) yi (pop z-list) ) radius f output-stream)
               (clockwise-move-R (list (+ xi radius ) yi (pop z-list) ) radius f output-stream)
               ))
					;	      (clockwise-move-R (list (- xi radius ) yi (pop z-list) ) radius f output-stream)
    (goto (list xi yi zsafe) output-stream)
    ))
;; helical-drill-at-point ends here




;; [helical-drill-array]
(defun helical-drill-array (point-array zsafe zstart zend zstep radius f output-stream)
  "helical-drill-array: we drill on an array of points
`point-array' list of points to drill
`zsafe' z safe
`zstart' z start
`zend' z end
`zstep' step at this point 
`radius' helica radius
`f' feedrate
`output-stream' where to write"
  (loop for point in point-array
	do (helical-drill point zsafe zstart zend zstep radius f output-stream)
	))

;; helical-drill-array ends here

;; drill function
;; [drill-at-point]
(defun drill-point (point zsafe zstart zend f- f+ output-stream)
  "drill-point drilling a point
`point' point to drill z-ommited
`zsafe' safe Z
`zstart' start Z
`zend' Z end
`f-'  feedrate
`f+' feedrate not used
`output-stream' where to write)"
  (let* ((xi (x-of point))
         (yi (y-of point))
	 (point-safe (list xi yi zsafe))
	 (point-start (list xi yi zstart))
	 (point-end (list xi yi zend)))
    
    (format output-stream (concatenate 'string
				     *crlf*
				     (format nil "(drilling point: X~8,3F Y~8,3F)" xi yi)
				     *crlf*))
    (goto point-safe output-stream)
    (goto point-start output-stream)
    (linear-move point-end f- output-stream)
    (linear-move point-start f+ output-stream)
    (goto point-safe output-stream)
    ))
;; drill-at-point ends here

;; [drill-array]
(defun drill-point-array (point-array zsafe zstart zend f- f+ output-stream)
  "drill-point-array simple drilling an array of points
`point-array' point-array to drill z-ommited
`zsafe' safe Z
`zstart' start Z
`zend' Z end
`f-'  feedrate
`f+' feedrate not used
`output-stream' where to write)"
  (loop for point in point-array
        do (drill-point point zsafe zstart zend f- f+ output-stream)
        ))
;; drill-array ends here

;; [point-couples]
(defun point-couples (center radius trochoidal-width no-of-points)
  "point couples returns offset pairs internal-external of circle points
`center' center of circle
`radius' radius of circle
`trochoidal-width' distance between internal external points
`no-of-points' number of points"
    (let* ((internal-points (divide-circle center radius no-of-points))
           (external-points (divide-circle center (+ radius trochoidal-width) no-of-points))
           (couples (mapcar #'list internal-points external-points)))
      couples))

(defun point-couples-arc (center radius start-angle end-angle direction trochoidal-width no-of-points)
  "point couples returns offset pairs internal-external of circle points
`center' center of circle
`radius' radius of circle
`start-angle' start angle
`end-angle' end angle
`direction' direction (cw or ccw)
`trochoidal-width' distance between internal external points
`no-of-points' number of points"
    (let* ((internal-points (divide-arc center radius start-angle end-angle direction no-of-points))
           (external-points (divide-arc center (+ radius trochoidal-width) start-angle end-angle direction  no-of-points))
           (couples (mapcar #'list internal-points external-points)))
      couples))

