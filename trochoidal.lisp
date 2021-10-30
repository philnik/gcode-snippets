;; trochoidal-circle
  
;; #+NAME: trochoidal-circle-refactor
;; #+HEADER: :tangle trochoidal.lisp :noweb yes
;; #+HEADER: :results value
;; #+HEADER: :comments both :cache yes :session step

;; [[file:step.org::trochoidal-circle-refactor][trochoidal-circle-refactor]]
(defun trochoidal-data-gcode-prologue (l-step zi f- center  tool-diameter radius trochoidal-width z-list-in output-stream)
  (progn  
    (format output-stream "(l-step: ~f  mm)~%" l-step)
    (format output-stream "(Zcut: Z~f )~%" zi)
    (format output-stream "(Z-list-in: ~{Z~a~^, ~})~%" z-list-in)
    (format output-stream "(feedrate: ~f mm/min) ~%" f-)
    (format output-stream "~%")

    (format output-stream "(Center: X~5$ mm  Y~5$)~%" (car center) (cadr center))

    (format output-stream "~%")
    
    (format output-stream "(tool-diameter: ~3$ mm) ~%" tool-diameter)
    (format output-stream "(trochoidal-width: ~3$ mm) ~%" trochoidal-width)

    (format output-stream "~%")
    (format output-stream "(Internal cutting radius: ~3$ mm)~%" (- radius (/ tool-diameter 2.0)))
    (format output-stream "(External cutting radius: ~3$ mm)~%" (+ radius trochoidal-width (/ tool-diameter 2.0)))

    (format output-stream "~%")
    (format output-stream "(Internal cutting diameter: ~3$ mm)~%" (* 2.0 (- radius (/ tool-diameter 2.0))))
    (format output-stream "(External cutting diameter: ~3$ mm)~%" (* 2.0 (+ radius trochoidal-width (/ tool-diameter 2.0))))

    (format output-stream "~%")
    ))

(defmacro helical-move (start-pair z-list fz output-stream move-R)
  "helical-moving-function
`start-pair' pair of points - vector in-point->out-point, start from first-point
`z-list' list of z height points - each forms a half circle
`fz' feedrate
`output-stream' stream-to-write
`move-R' function to run, accepted values ccw-move-R/cw-move-R"
`(let* ((m2p (middle-point (car start-pair) (cadr start-pair)))
       (start-point (append m2p (list (car z-list))))
       (trocho-radius (* (length-vector ,start-pair) 0.5d0)))
  (format output-stream "~%(helical drilling: X~8,3F Y~8,3F R~8,3F)~%" (x-of start-point) (y-of start-point) trocho-radius)
  (loop while ,z-list
	do (let* ((z (pop ,z-list))
		  (in-point (append (car start-pair) (list z)))
		  (out-point (append (cadr start-pair) (list z))))
	     (,move-R in-point trocho-radius ,fz output-stream)
	     (,move-R out-point trocho-radius  ,fz output-stream)
	     ))))

(defun internal-trochoidal-circle (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  "Outputs a trochoidal path circle in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`center' center of the circle
`tool-diameter' tool diameter
`internal-diameter' inside diameter
`external-diameter' outside diameter
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (radius (/(+ internal-diameter tool-diameter)2.0))
	 (trochoidal-width (- (/(- external-diameter internal-diameter)2.0) tool-diameter))
	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (no-of-points (no-of-points-from-arc-radius xystep radius))
	 (couple (point-couples center radius trochoidal-width no-of-points))

	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe)))
	 )

    (progn 
      (goto start-point output-stream)
      (format output-stream "~%(helical drilling: X~8,3F Y~8,3F)~%" (x-of start-point) (y-of start-point))
      (loop while z-list
	    do (let* ((z (pop z-list))
		      (in-point (append (car start-pair) (list z)))
		      (out-point (append (cadr start-pair) (list z))))
		 (cw-move-R out-point trochoidal-radius fz output-stream)
		 (cw-move-R in-point trochoidal-radius  fz output-stream)
		 ))
      
      (format output-stream "~% (trochoidal circle cycle)~%")
      
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (ccw-move-R in-point radius  fxy output-stream)
		 (cw-move-R out-point trochoidal-radius fxy output-stream)
		 (cw-move-R in-point trochoidal-radius  fxy output-stream)
		 ))

      (let* ((in-point (append (car start-pair) (list zend)))
	     (out-point (append (cadr start-pair) (list zend))))
	(progn
	  (ccw-move-R in-point radius  fxy output-stream)
	  (cw-move-R out-point trochoidal-radius fxy output-stream)
	  (cw-move-R in-point trochoidal-radius  fxy output-stream)
	))
      
      (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of start-point) (y-of start-point))
      (loop while z+list
	    do (let* ((z (pop z+list))
		      (in-point (append (car start-pair) (list z)))
		      (out-point (append (cadr start-pair) (list z))))
		 (cw-move-R out-point trochoidal-radius fz output-stream)
		 (cw-move-R in-point trochoidal-radius  fz output-stream)
	       ))
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))


(defun external-trochoidal-circle (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  "Outputs a trochoidal path circle in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`center' center of the circle
`tool-diameter' tool diameter
`internal-diameter' inside diameter
`external-diameter' outside diameter
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (radius (/(+ internal-diameter tool-diameter)2.0))
	 (radius1 (/(- external-diameter tool-diameter)2.0))
	 (trochoidal-width (- (/(- external-diameter internal-diameter)2.0) tool-diameter))
	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (no-of-points (no-of-points-from-arc-radius xystep radius))
	 (couple (point-couples center radius trochoidal-width no-of-points))

	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe)))
	 )
    (progn 
      (goto start-point output-stream)
      (format output-stream "~%(helical drilling: X~8,3F Y~8,3F)~%" (x-of start-point) (y-of start-point))
      (loop while z-list
	    do (let* ((z (pop z-list))
		      (in-point (append (car start-pair) (list z)))
		      (out-point (append (cadr start-pair) (list z))))
		 (ccw-move-R in-point trochoidal-radius fz output-stream)
		 (ccw-move-R out-point trochoidal-radius  fz output-stream)
		 ))
      
      (format output-stream "~% (trochoidal circle cycle)~%")
      
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (ccw-move-R out-point radius1  fxy output-stream)
		 (ccw-move-R in-point trochoidal-radius fxy output-stream)
		 (ccw-move-R out-point trochoidal-radius  fxy output-stream)
		 ))

      (let* ((in-point (append (car start-pair) (list zend)))
	     (out-point (append (cadr start-pair) (list zend))))
	(progn
	  (ccw-move-R out-point radius1  fxy output-stream)
	  (ccw-move-R in-point trochoidal-radius fxy output-stream)
	  (ccw-move-R out-point trochoidal-radius  fxy output-stream)
	))
      
      (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of start-point) (y-of start-point))
      (loop while z+list
	    do (let* ((z (pop z+list))
		      (in-point (append (car start-pair) (list z)))
		      (out-point (append (cadr start-pair) (list z))))
		 (ccw-move-R in-point trochoidal-radius fz output-stream)
		 (ccw-move-R out-point trochoidal-radius  fz output-stream)
	       ))
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))

(defun external-D-trochoidal-circle (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  "Outputs a trochoidal D-type path circle in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`center' center of the circle
`tool-diameter' tool diameter
`internal-diameter' inside diameter
`external-diameter' outside diameter
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))
	 (radius (/(+ internal-diameter tool-diameter)2.0))
	 (radius1 (/(- external-diameter tool-diameter)2.0))
	 (trochoidal-width (- (/(- external-diameter internal-diameter)2.0) tool-diameter))
	 (trochoidal-radius (/ trochoidal-width 2.0))
	 (no-of-points (no-of-points-from-arc-radius xystep radius))
	 (couple (point-couples center radius trochoidal-width no-of-points))

	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe))))

    (progn 
      (goto start-point output-stream)

      (helical-move start-pair z-list fz output-stream ccw-move-R)
		    
      (format output-stream "~% (trochoidal circle cycle)~%")
      
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (ccw-move-R out-point radius1  fxy output-stream)

		 (ccw-move-R in-point trochoidal-radius fxy output-stream)
		 
		 (let* ((dr-vector1 (normal-vector (list out-point in-point)))
			(dtheta-vector1 (vector-rotate dr-vector1 (* *pi* 0.5d0)))
			(point1 (append (p+ (p+ out-point (nth 1 dr-vector1)) (nth 1 dtheta-vector1)) (list zend)))
			(dr-vector2 (normal-vector (list in-point out-point)))
			(dtheta-vector2 (vector-rotate dr-vector2 (* *pi* -0.5d0)))
			(point2 (append (p+ (p+ in-point (nth 1 dr-vector2)) (nth 1 dtheta-vector2)) (list zend))))
		       
;		   (ccw-move-R point2 1.0 fxy output-stream)
;		   (linear-move (append point1 (list zend)) fxy output-stream)
		   (linear-move out-point fxy output-stream)
;		   (ccw-move-R out-point 1.0  fxy output-stream)
		   )))
      

      (let* ((in-point (append (car start-pair) (list zend)))
	     (out-point (append (cadr start-pair) (list zend))))
	(progn
	  (ccw-move-R out-point radius1  fxy output-stream)
	  (ccw-move-R in-point trochoidal-radius fxy output-stream)
	  (ccw-move-R out-point trochoidal-radius  fxy output-stream)
	))

      (helical-move start-pair z+list fz output-stream ccw-move-R)
		    
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))





(defun external-D2-trochoidal-circle (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  "Outputs a trochoidal D-type path circle in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`center' center of the circle
`tool-diameter' tool diameter
`internal-diameter' inside diameter
`external-diameter' outside diameter
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))
	 (radius (/(+ internal-diameter tool-diameter)2.0))
	 (radius1 (/(- external-diameter tool-diameter)2.0))
	 (trochoidal-width (- (/(- external-diameter internal-diameter)2.0) tool-diameter))
	 (trochoidal-radius (/ trochoidal-width 2.0))
	 (no-of-points (no-of-points-from-arc-radius xystep radius))
	 (couple (point-couples center radius trochoidal-width no-of-points))

	 (dradius 1.0)
	 
	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe))))

    (progn 
      (goto start-point output-stream)

      (helical-move start-pair z-list fz output-stream ccw-move-R)
		    
      (format output-stream "~% (trochoidal circle cycle)~%")
      
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (ccw-move-R out-point radius1  fxy output-stream)

		 (ccw-move-R in-point trochoidal-radius fxy output-stream)
		 
		 (let* ((dr-vector1 (vector-scale (normal-vector (list out-point in-point)) dradius))
			(dtheta-vector1 (vector-rotate dr-vector1 (* *pi* 0.5d0)))
			(point1 (append (p+ (p+ out-point (nth 1 dr-vector1)) (nth 1 dtheta-vector1)) (list zend)))
			(dr-vector2 (vector-scale (normal-vector (list in-point out-point)) dradius))
			(dtheta-vector2 (vector-rotate dr-vector2 (* *pi* -0.5d0)))
			(point2 (append (p+ (p+ in-point (nth 1 dr-vector2)) (nth 1 dtheta-vector2)) (list zend))))
		       
		   (ccw-move-R point2 dradius fxy output-stream)
		   (linear-move (append point1 (list zend)) fxy output-stream)
;		   (linear-move out-point fxy output-stream)
		   (ccw-move-R out-point dradius fxy output-stream)
		   )))
      

      (let* ((in-point (append (car start-pair) (list zend)))
	     (out-point (append (cadr start-pair) (list zend))))
	(progn
	  (ccw-move-R out-point radius1  fxy output-stream)
	  (ccw-move-R in-point trochoidal-radius fxy output-stream)
	  (ccw-move-R out-point trochoidal-radius  fxy output-stream)
	))

      (helical-move start-pair z+list fz output-stream ccw-move-R)
		    
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))
