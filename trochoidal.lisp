;; trochoidal-circle
  
;; #+NAME: trochoidal-circle-refactor
;; #+HEADER: :tangle trochoidal.lisp :noweb yes
;; #+HEADER: :results value
;; #+HEADER: :comments both :cache yes :session step

;; [[file:step.org::trochoidal-circle-refactor][trochoidal-circle-refactor]]
(defun trochoidal-data-gcode-prologue (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  "prints out data for trochoidal file
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
  (format output-stream
d	  (concatenate 'string 
		       (format nil "(xy-step [trochoidal step]: ~f  mm)" xystep) *crlf*
		       (format nil "(trochoidal width: ~f  mm)" (* (- external-diameter internal-diameter) 0.5)) *crlf*
		       *crlf*
		       (format nil "(zsafe [ Safe height]: Z~f mm )" zsafe) *crlf*
		       (format nil "(zstart [ Start helical movement from here]: Z~f mm )" zstart) *crlf*
		       (format nil "(helical z feedrate: ~f mm/min)" fz) *crlf*
		       (format nil "(fxy feedrate: ~f mm/min)" fxy) *crlf*
		       *crlf*
		       (format nil "(Center of the trochoidal circle: X~5$ mm  Y~5$)" (car center) (cadr center)) *crlf*
		       *crlf*    
		       (format nil "(tool-diameter: ~3$ mm)" tool-diameter) *crlf*
		       (format nil "(internal diameter of trochoidal circle: ~3$ mm)" internal-diameter) *crlf*
		       (format nil "(external diameter of trochoidal circle: ~3$ mm)" external-diameter) *crlf*
		       *crlf*
		       (format nil "(Internal cutting radius: ~3$ mm)" (/ internal-diameter 2.0)) *crlf*
		       (format nil "(External cutting radius: ~3$ mm)" (/ external-diameter 2.0)) *crlf*
		       *crlf*
		       ))
  )
  

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
   (format output-stream
	   (concatenate 'string
			*crlf*
			(format nil "(helical drilling: X~8,3F Y~8,3F R~8,3F)" (x-of start-point) (y-of start-point) trocho-radius)
			*crlf*
			))
  (loop while ,z-list
	do (let* ((z (pop ,z-list))
		  (in-point (append (car start-pair) (list z)))
		  (out-point (append (cadr start-pair) (list z))))
	     (,move-R in-point trocho-radius ,fz ,output-stream)
	     (,move-R out-point trocho-radius  ,fz ,output-stream)
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


(defun external-D-trochoidal-circle (xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream &optional (dradius 2.0))
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
`dradius' corner radius default to 2.0
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


(defun external-trochoidal-line (xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
  "Outputs a trochoidal line in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`tool-diameter' tool diameter
`vector' vector describing line
`trochoidal-width' trochoidal-width
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (couple (line-point-couples vector trochoidal-width xystep))

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
      
      (format output-stream "~% (trochoidal line cycle)~%")
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (linear-move out-point fxy output-stream)
		 (ccw-move-R in-point trochoidal-radius fxy output-stream)
		 (ccw-move-R out-point trochoidal-radius  fxy output-stream)
		 ))

      
      (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of end-point) (y-of end-point))
      (loop while z+list
	    do (let* ((z (pop z+list))
		      (in-point (append (car end-pair) (list z)))
		      (out-point (append (cadr end-pair) (list z))))
		 (ccw-move-R in-point trochoidal-radius fz output-stream)
		 (ccw-move-R out-point trochoidal-radius  fz output-stream)
	       ))
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))


(defun helical-dive (xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
    (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (couple (line-point-couples vector trochoidal-width xystep))

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
	       )))))


(defun helical-raise (xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
    (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (couple (line-point-couples vector trochoidal-width xystep))

	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe)))
	 )
(progn
  (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of end-point) (y-of end-point))
  (loop while z+list
	do (let* ((z (pop z+list))
		  (in-point (append (car end-pair) (list z)))
		  (out-point (append (cadr end-pair) (list z))))
	     (ccw-move-R in-point trochoidal-radius fz output-stream)
	     (ccw-move-R out-point trochoidal-radius  fz output-stream)
	     )))))




(defun external-trochoidal-line-2 (xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
  "Outputs a trochoidal line in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`tool-diameter' tool diameter
`vector' vector describing line
`trochoidal-width' trochoidal-width
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (couple (line-point-couples vector trochoidal-width xystep))

	 (start-pair (pop couple))
	 (start-point (append (middle-vector start-pair) (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (middle-vector end-pair) (list zsafe)))
	 )
    (progn 
;      (helical-dive xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
      
      (format output-stream "~% (trochoidal line cycle)~%")
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
		 (linear-move out-point fxy output-stream)
		 (ccw-move-R in-point trochoidal-radius fxy output-stream)
		 (ccw-move-R out-point trochoidal-radius  fxy output-stream)
		 ))

      
;      (helical-raise xystep zstep zsafe zstart zend fz fxy tool-diameter vector trochoidal-width output-stream)
      
      (format output-stream "~% (end of cycle)~%")
;      (goto start-point output-stream)
      )))






(defun external-D-trochoidal-line (xystep zstep zsafe zstart zend
				   fz fxy tool-diameter vector trochoidal-width output-stream &optional (dradius 2.0))
  "Outputs a trochoidal line in gcode
`xystep' stepping of each circle
`zstep' helical step
`zsafe' zsafe
`zstart' zstart
`zend' zend
`fz' helical z feedrate
`fxy' xy feedrate
`tool-diameter' tool diameter
`vector' vector describing line
`trochoidal-width' trochoidal-width
`output-stream' where to output the g-code
"
  (let* (
	 (z+list (z-step-list zstart zend zstep))
	 (z-list (z-step-list zend zstart (- 0 zstep)))

	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (couple (line-point-couples vector trochoidal-width xystep))

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
      
      (format output-stream "~% (trochoidal line cycle)~%")
      ;;;;couple

      
      (loop while couple
	    do (let* ((pair (pop couple))
		      (in-point (append (car pair) (list zend)))
		      (out-point (append (cadr pair) (list zend)))
		      )
;		 (ccw-move-R out-point radius1  fxy output-stream)

		 (ccw-move-R in-point trochoidal-width fxy output-stream)
		 
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
      
      ;;;couple
      
      (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of end-point) (y-of end-point))
      (loop while z+list
	    do (let* ((z (pop z+list))
		      (in-point (append (car end-pair) (list z)))
		      (out-point (append (cadr end-pair) (list z))))
		 (ccw-move-R in-point trochoidal-radius fz output-stream)
		 (ccw-move-R out-point trochoidal-radius  fz output-stream)
	       ))
      (format output-stream "~% (end of cycle)~%")
      (goto start-point output-stream)
      )))




(defclass trocho-arc ()
  (
   (xystep
    :accessor xystep)
   (zstep
    :accessor zstep)
   zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
  )

(defun external-trochoidal-arc (xystep zstep
				zsafe zstart zend
				fz fxy
				center tool-diameter internal-diameter external-diameter
				start-angle end-angle direction
				output-stream)
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
`start-angle'
`end-angle'
`direction'
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

