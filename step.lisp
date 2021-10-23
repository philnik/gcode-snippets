
(defconstant *pi* 3.141592653589793d0)
(defconstant *2pi* 6.283185307179586d0)

(defun x-of (point) (nth 0 point))
(defun y-of (point) (nth 1 point))
(defun z-of (point) (nth 2 point))

  (defun rad-to-deg (a)
    (* (/ 180.0d0 *pi*) a))

  (defun deg-to-rad (a)
    (/ a (/ 180.0d0 *pi*)))
;; math-functions ends here

;; vector functions

;; #+name: vector-functions

;; [[file:step.org::vector-functions][vector-functions]]
(defun no-of-points-from-arc-radius (steps radius)
    "we get the no-of-points to divide circle"
    (let* ((divide (/ (* *2pi* radius) steps))
           )
                  ;;;turn to integer and find the closest to make 4-quarters
      (* (round (/ divide 8.0))8))
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

  (defun normal-vector (v)
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
;; vector-functions ends here

;; Arc functions  
;; #+name: arc-functions

;; [[file:step.org::arc-functions][arc-functions]]
(setq arc-3points  (list '(0.0d0 0.0d0) '(0.0d0 1.0d0) '(1.0d0 0.0d0)))
(setq arc-by-radius  (list 100.0 '(0.0d0 1.0d0) '(1.0d0 0.0d0)))

(defun radius-arc (arc-cen-2p)  ;; untested
  "we will calculate radius of an arc"
  "actually this radius is the distance from first point to each of the points"
  (let* ((center (nth 0 arc-cen-2p))
	 (start-point (nth 1 arc-cen-2p))
	 (end-point (nth 2 arc-cen-2p))
	 (v1 (list center start-point))
	 (v2 (list center end-point))
	 )
    (if ((- (length v1) (length v2)) 0.0)
	(format t "error can not form arc from points and center")
	(length v1)
	)))
    
(defun center-arc (arc-rad)
)  

(defun offset-arc (arc)
  )
;; arc-functions ends here

;; streams
;; #+name: streams

;; [[file:step.org::streams][streams]]
(defun prologue (spindle output-stream)
    (format output-stream "(prologue)~% M3~% M5~% M9~% S~d ~% ~% (end of prologue)~%" spindle)
    )
  (defun epilogue (output-stream)
    (format output-stream "~%  (epilogue) M3~% M5~% M30~% ~% %% ~% (end of program)~% %%")
    )
;; streams ends here

;; move-functions

;; #+NAME: move-functions

;; [[file:step.org::move-functions][move-functions]]
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

;;   not used it is better to choose the points on an array
;; #+NAME: step-function

;; [[file:step.org::step-function][step-function]]
(defun step-over (z0 z1 step)
    (let* ((dz (- z1 z0))
           (no-steps(+ 0 (floor (/ dz step))))
           (result '())
           )
      (push z1 result)
      (dotimes (n no-steps)
        (push (- (car result) step) result)
        )
      (if (/= z0 (car result))
          (push z0 result))
      result)
    )
;; step-function ends here

;; divide circle

;; Divide a circle on points

;; #+name: divide-circle

;; [[file:step.org::divide-circle][divide-circle]]
(defun polar-to-rect (center radius angle)
    (list
     (+ (x-of center) (* radius (cos angle)))
     (+ (y-of center) (* radius (sin angle)))
     ))

    (defun divide-circle (center radius no-of-points)
    ;;center: center of circle
    ;;radius: radius we move on
    ;;no-of-points: number of points
      (let ((angle (/ *2pi* no-of-points))
            (point-array '()))

        (dotimes (n (+ 1 no-of-points))
          (push (polar-to-rect center radius (* n angle)) point-array)
          )
      point-array
        ))
;; divide-circle ends here

;; define helical-drill function
  
;; #+NAME: helical-drill-at-point

;; [[file:step.org::helical-drill-at-point][helical-drill-at-point]]
(defun helical-drill (point z-list radius f- f+ output-stream)
            (let ((xi (x-of point))
                  (yi (y-of point))
                  (zstart (car z-list))
                  (zend (car (last z-list))))

              (format output-stream "~%(helical drilling point: X~8,3F Y~8,3F)~%" (x-of point) (y-of point))

              (goto (list (+ xi radius ) yi (pop z-list) ) output-stream)

              (loop while z-list
                    do (progn
                         (clockwise-move-R (list (- xi radius ) yi (pop z-list) ) radius f- output-stream)
                         (if (cdr z-list)
                             (clockwise-move-R (list (+ xi radius ) yi (pop z-list) ) radius f- output-stream)
                             (linear-move (list xi yi (pop z-list)) f- output-stream)
                             )))
              )
            )
;; helical-drill-at-point ends here

;; define drill array

;; #+name: helical-drill-array

;; [[file:step.org::helical-drill-array][helical-drill-array]]
(defun helical-drill-array (point-array z-list radius f- f+ output-stream)
    (loop for point in point-array
          do (helical-drill point z-list radius f- f+ output-stream)
          ))
;; helical-drill-array ends here

;; define simple drill function
  
;; #+NAME: drill-at-point

;; [[file:step.org::drill-at-point][drill-at-point]]
(defun drill-point (point z-list f- f+ output-stream)
    (let ((xi (x-of point))
          (yi (y-of point))
          (zstart (car z-list))
          (zend (car (last z-list))))

      (format output-stream "~%(drilling point: X~8,3F Y~8,3F)~%" (x-of point) (y-of point))

      (goto (list xi yi (pop z-list)) output-stream)

      (loop while z-list
            do (progn
                 (linear-move (list xi yi (pop z-list)) f- output-stream)
                 (if (cdr z-list)
                     (linear-move (list xi yi (pop z-list)) f- output-stream)
                     (goto (list xi yi (pop z-list)) output-stream)
                     )))))
;; drill-at-point ends here

;; define drill array

;; #+name: drill-array

;; [[file:step.org::drill-array][drill-array]]
(defun drill-array (point-array z-list f- f+ output-stream)
    (loop for point in point-array
          do (drill-point point z-list f- f+ output-stream)
          ))
;; drill-array ends here

;; divide-circle-2
;; We work on trochoidal
;; We make an array of internal and external point of arcs this one is for moving on circle path.
;; We borrow from divide circle the points

;; #+name: divide-circle-2

;; [[file:step.org::divide-circle-2][divide-circle-2]]
(defun point-couples (center radius trochoidal-width no-of-points)
    (let* ((internal-points (divide-circle center radius no-of-points))
           (external-points (divide-circle center (+ radius trochoidal-width) no-of-points))
           (couples (mapcar #'list internal-points external-points)))
      couples))
;; divide-circle-2 ends here

;; external-trochoidal-circle
  
;; #+NAME: trochoidal-circle

;; [[file:step.org::trochoidal-circle][trochoidal-circle]]
(defun internal-trochoidal-circle (l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
      (progn
        (setq radius (- internal-radius trochoidal-width (/ tool-diameter 2.0))) 
          (setq no-of-points (no-of-points-from-arc-radius l-step radius))
          (setq couple (point-couples center radius trochoidal-width no-of-points))
          (setq z-list-out (reverse z-list-in))

          (prologue 10000 output-stream)
          (setf start-point (append (caar couple) (list zi)))
          (setq helical-start-center
                    (let* ((point-min (caar couple))
                             (point-max (cadar couple))
                             (x (* 0.5 (+ (x-of point-min) (x-of point-max))))
                             (y (* 0.5 (+ (y-of point-min) (y-of point-max))))
                             )
                        (list x y zi)
                        ))
          (helical-drill helical-start-center z-list-in (/ trochoidal-width 2.0) f- f- output-stream)
          (format output-stream "~% ~% (start trochoid)~%")
                              (dotimes (i (+ 1 no-of-points))
                                (let* ((pair (pop couple))
                                       (in-point (append (car pair) (list zi)))
                                       (out-point (append (cadr pair) (list zi)))
                                       (trochoidal-radius (/ trochoidal-width 2.0))
                                       )

                                  (cw-move-R in-point radius  f- output-stream)
                                  (ccw-move-R out-point trochoidal-radius f- output-stream)
                                  (ccw-move-R in-point trochoidal-radius  f- output-stream)
                                  ))

          (format output-stream "~% ~% (end trochoid)~%")
          (helical-drill helical-start-center z-list-out (/ trochoidal-width 2.0) f- f- output-stream)
          (epilogue output-stream)
          ))

  (defun external-trochoidal-circle (l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
    (progn
     (setq radius (+ internal-radius (/ tool-diameter 2.0))) 
     (setq no-of-points (no-of-points-from-arc-radius l-step radius))
     (setq couple (point-couples center radius trochoidal-width no-of-points))
     (setq z-list-out (reverse z-list-in))

     (prologue 10000 output-stream)
     (setf start-point (append (caar couple) (list zi)))

     (setq helical-start-center
           (let* ((point-min (caar couple))
                  (point-max (cadar couple))
                  (x (* 0.5 (+ (x-of point-min) (x-of point-max))))
                  (y (* 0.5 (+ (y-of point-min) (y-of point-max))))
                  )
             (list x y zi)
             ))
     (helical-drill helical-start-center z-list-in (/ trochoidal-width 2.0) f- f- output-stream)
     (format output-stream "~% ~% (start trochoid)~%")
     (dotimes (i (+ 1 no-of-points))
       (let* ((pair (pop couple))
              (in-point (append (car pair) (list zi)))
              (out-point (append (cadr pair) (list zi)))
              (trochoidal-radius (/ trochoidal-width 2.0))
              )
         ;;  (linear-move in-point  f- output-stream)
         (cw-move-R in-point radius  f- output-stream)
         (ccw-move-R out-point trochoidal-radius f- output-stream)
         (ccw-move-R in-point trochoidal-radius  f- output-stream)
         ))
     (format output-stream "~% ~% (end trochoid)~%")
     (helical-drill helical-start-center z-list-out (/ trochoidal-width 2.0) f- f- output-stream)
     (epilogue output-stream)
     ))

  (defun external-trochoidal-circle-array (l-step zi f- center-array tool-diameter internal-radius trochoidal-width z-list-in output-stream)
    (progn
     (setq radius (+ internal-radius (/ tool-diameter 2.0))) 
     (setq no-of-points (no-of-points-from-arc-radius l-step radius))

     (setq z-list-out (reverse z-list-in))

     (prologue 10000 output-stream)

     (loop for center in center-array
	   do (progn
		(setq couple (point-couples center radius trochoidal-width no-of-points))
		(setf start-point (append (caar couple) (list zi)))
		
		(setq helical-start-center
		      (let* ((point-min (caar couple))
			     (point-max (cadar couple))
			     (x (* 0.5 (+ (x-of point-min) (x-of point-max))))
			     (y (* 0.5 (+ (y-of point-min) (y-of point-max))))
			     )
			(list x y zi)
			))
		(helical-drill helical-start-center z-list-in (/ trochoidal-width 2.0) f- f- output-stream)
		(format output-stream "~% ~% (start trochoid)~%")
		(dotimes (i (+ 1 no-of-points))
		  (let* ((pair (pop couple))
			 (in-point (append (car pair) (list zi)))
			 (out-point (append (cadr pair) (list zi)))
			 (trochoidal-radius (/ trochoidal-width 2.0))
			 )
         ;;  (linear-move in-point  f- output-stream)
		    (cw-move-R in-point radius  f- output-stream)
		    (ccw-move-R out-point trochoidal-radius f- output-stream)
		    (ccw-move-R in-point trochoidal-radius  f- output-stream)
         ))
		(format output-stream "~% ~% (end trochoid)~%")
		(helical-drill helical-start-center z-list-out (/ trochoidal-width 2.0) f- f- output-stream)
		))
     (epilogue output-stream)
     ))
;; trochoidal-circle ends here
