
(defconstant *pi* 3.141592653589793d0)
(defconstant *2pi* 6.283185307179586d0)

(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

  (defun rad-to-deg (a)
    (* (/ 180.0d0 *pi*) a))

  (defun deg-to-rad (a)
    (/ a (/ 180.0d0 *pi*)))

;; #+name: vector-functions

(defun no-of-points-from-arc-radius (steps radius)
    "we get the no-of-points to divide circle"
    (let* ((divide (/ (* *2pi* radius) steps))
           )
                  ;;;turn to integer and find the closest to make 4-quarters
      (* (round (/ divide 1.0))1))
    )
  (defun length-vector (v)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           )
      (sqrt (+ (expt (- p2x p1x) 2.0d0) (expt (- p2y p1y) 2.0d0)))
      ))

  (defun normal-vector-2 (v)
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
                 (atan (/ (- p2y p1y) (- p2x p1x)))
                 )))
      (list '(0.0d0 0.0d0) (list (cos angle) (sin angle)))
      ))

  (defun normal-vector (v)
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
          (atan (/ (- p2y p1y) (- p2x p1x))))
      ))

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
  (middle-point (nth 0 v) (nth 1 v)))

  (defun vector-scale (v scale-factor)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (length (length-vector v))
           (angle (angle-vector v)))
      (list p1 (list (+ p1x (* scale-factor (cos angle))) (+ p1y (* scale-factor (sin angle)))))
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

;; [[file:step.org::arc-functions][arc-functions]]
(setq arc-3points  (list '(0.0d0 0.0d0) '(0.0d0 1.0d0) '(1.0d0 0.0d0)))
(setq arc-by-radius  (list 100.0 '(0.0d0 1.0d0) '(1.0d0 0.0d0)))

(defun radius-arc (arc-cen-2p)  ;; untested
  "we will calculate radius of an arc"
  (let* ((center (nth 0 arc-cen-2p))
	 (start-point (nth 1 arc-cen-2p))
	 (end-point (nth 2 arc-cen-2p))
	 (v1 (list center start-point))
	 (v2 (list center end-point))
	 )
    (if (> (- (length v1) (length v2)) 0.0)
	(format t "error can not form arc from points and center")
	(length v1)
	)))
    
(defun center-arc (arc-rad)
)  

(defun offset-arc (arc)
  )

;; [[file:step.org::streams][streams]]
(defun prologue (spindle output-stream)
    (format output-stream "(prologue)~% M03 S~d ~% ~% (end of prologue)~%" spindle)
    )
  (defun epilogue (output-stream)
    (format output-stream "~%  (epilogue) ~% M05~% M30~% ~% (end of program)~% %%")
)

;; [move-functions]
(defun goto (point str)
  (format str "G0 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T ~%" (x-of point) (y-of point) (z-of point)))

(defun linear-move (point f str)
  (format str "G1 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F ~35T F~D ~%" (x-of point) (y-of point) (z-of point) f))

(defun clockwise-move-ij (point i j f str)
  (format str "G2 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  I~8,3F ~35T J~8,3F ~45T F~D ~%" (x-of point) (y-of point) (z-of point) i j f))

(defun clockwise-move-R (point r f str)
  (format str "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(defun cw-move-R (point r f str)
  "Clockwise interpolation G03 gcode string
`point' endpoint of the arc
`r' radius of the arc
`f' feedrate of the move
`str' output-stream"
  (format str "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(defun ccw-move-R (point r f str)
"Counter clockwise interpolation G03 gcode string
`point' endpoint of the arc
`r' radius of the arc
`f' feedrate of the move
`str' output-stream"
  (format str "G03 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

(defun counter-clockwise-move-R (point r f str)
  "G03 interpolations calls ccw-move-R"
  (ccw-move-R point r f str))


;; move-functions ends here

;; Step function


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

;; step-function ends here


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
;; divide-circle ends here

(defun divide-line (vector no-of-points)
  "divides a line to points
`vector' descibes start-point->end-point of line
`no-of-points' number of points"
  (let* ((l (length-vector vector))
	 (step (/ l no-of-points))
	 (start-point (car vector))
	 (end-point (cadr vector))
         (point-array '()))

        (dotimes (n  no-of-points)
          (push (polar-to-rect center radius (* n angle)) point-array)
          )
      (reverse point-array)
        ))
;; divide-circle ends here





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

    (format output-stream "~%(helical drilling point: X~8,3F Y~8,3F)~%" (x-of point) (y-of point))

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
    
    (format output-stream "~%(drilling point: X~8,3F Y~8,3F)~%" xi yi)
    (goto point-safe output-stream)
    (goto point-start output-stream)
    (linear-move point-end f- output-stream)
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

