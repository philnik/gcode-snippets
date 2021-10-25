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
	 (start-point (append
			    (middle-point (car start-pair) (cadr start-pair))
			    (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (car end-pair) (list zsafe))))

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
      
      (format output-stream "~%(helical out: X~8,3F Y~8,3F)~%" (x-of end-point) (y-of end-point))
      (loop while z+list
	    do (let* ((z (pop z+list))
		      (in-point (append (car end-pair) (list z)))
		      (out-point (append (cadr end-pair) (list z))))
		 (cw-move-R out-point trochoidal-radius fz output-stream)
		 (cw-move-R in-point trochoidal-radius  fz output-stream)
	       ))
      (format output-stream "~% (end of cycle)~%")
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

	 (radius (/(- external-diameter tool-diameter)2.0))
	 (radius1 (/(+ internal-diameter tool-diameter)2.0))
	 (trochoidal-width (- (/(- external-diameter internal-diameter)2.0) tool-diameter))
	 (trochoidal-radius (/ trochoidal-width 2.0))
	 
	 (no-of-points (no-of-points-from-arc-radius xystep radius))
	 (couple (point-couples center radius1 trochoidal-width no-of-points))

	 (start-pair (pop couple))
	 (start-point (append
			    (middle-point (car start-pair) (cadr start-pair))
			    (list zsafe)))
	 
	 (end-pair (car(last couple)))
	 (end-point (append (car end-pair) (list zsafe))))

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
		 (ccw-move-R out-point radius  fxy output-stream)
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
      )))
