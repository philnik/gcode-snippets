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

(defun trochoidal-circle (l-step zi f- center tool-diameter radius trochoidal-width z-list-in output-stream)
"Outputs a trochoidal path circle in gcode
`L-STEP' stepping of each circle
`zi' cutting z
`center' center of gthe circle
`tool-diameter' tool diameter
`radius' internal radius of the trochoidal path
`trochoidal-width' trochoidal width
`z-list-in' list of the values of the helical drilling
`output-stream' where to output the g-code
"
  (progn
    (setq no-of-points (no-of-points-from-arc-radius l-step radius))
    (setq couple (point-couples center radius trochoidal-width no-of-points))
    (setq z-list-out (reverse z-list-in))
    (setq trochoidal-radius (/ trochoidal-width 2.0))
    (setf start-point (append (caar couple) (list zi)))
    (setq helical-start-center
          (let* ((point-min (caar couple))
                 (point-max (cadar couple))
                 (x (* 0.5 (+ (x-of point-min) (x-of point-max))))
                 (y (* 0.5 (+ (y-of point-min) (y-of point-max))))
                 )
            (list x y zi)
            ))

    (trochoidal-data-gcode-prologue l-step zi f- center tool-diameter radius trochoidal-width z-list-in output-stream)

    (format output-stream "~% (helical drilling cutting cycle)~%")
    (helical-drill helical-start-center z-list-in trochoidal-radius  f- f- output-stream)

    (format output-stream "~% (trochoidal circle cycle)~%")

    (dotimes (i (+ 1 no-of-points))
      (let* ((pair (pop couple))
             (in-point (append (car pair) (list zi)))
             (out-point (append (cadr pair) (list zi)))
             )
	(cw-move-R in-point radius  f- output-stream)
        (ccw-move-R out-point trochoidal-radius f- output-stream)
        (ccw-move-R in-point trochoidal-radius  f- output-stream)
        ))
    (format output-stream "~% (end trochoid)~%")
    (format output-stream "~% (helical drilling retracting cycle)~%")
    (helical-drill helical-start-center z-list-out (/ trochoidal-width 2.0) f- f- output-stream)
    (format output-stream "~% (end of cycle)~%")
    ))




(defun trochoidal-circle (l-step zi f- center tool-diameter radius trochoidal-width z-list-in output-stream)
"Outputs a trochoidal path circle in gcode
`L-STEP' stepping of each circle
`zi' cutting z
`center' center of gthe circle
`tool-diameter' tool diameter
`radius' internal radius of the trochoidal path
`trochoidal-width' trochoidal width
`z-list-in' list of the values of the helical drilling
`output-stream' where to output the g-code
"
  (progn
    (setq no-of-points (no-of-points-from-arc-radius l-step radius))
    (setq couple (point-couples center radius trochoidal-width no-of-points))
    (setq z-list-out (reverse z-list-in))
    (setq trochoidal-radius (/ trochoidal-width 2.0))
    (setf start-point (append (caar couple) (list zi)))
    (setq helical-start-center
          (let* ((point-min (caar couple))
                 (point-max (cadar couple))
                 (x (* 0.5 (+ (x-of point-min) (x-of point-max))))
                 (y (* 0.5 (+ (y-of point-min) (y-of point-max))))
                 )
            (list x y zi)
            ))

    (trochoidal-data-gcode-prologue l-step zi f- center tool-diameter radius trochoidal-width z-list-in output-stream)

    (format output-stream "~% (helical drilling cutting cycle)~%")
    (helical-drill helical-start-center z-list-in trochoidal-radius  f- f- output-stream)

    (format output-stream "~% (trochoidal circle cycle)~%")

    (dotimes (i (+ 1 no-of-points))
      (let* ((pair (pop couple))
             (in-point (append (car pair) (list zi)))
             (out-point (append (cadr pair) (list zi)))
             )
	(cw-move-R in-point radius  f- output-stream)
        (ccw-move-R out-point trochoidal-radius f- output-stream)
        (ccw-move-R in-point trochoidal-radius  f- output-stream)
        ))
    (format output-stream "~% (end trochoid)~%")
    (format output-stream "~% (helical drilling retracting cycle)~%")
    (helical-drill helical-start-center z-list-out (/ trochoidal-width 2.0) f- f- output-stream)
    (format output-stream "~% (end of cycle)~%")
    ))






