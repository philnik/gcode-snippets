;; quicklisp functions


;; #+name: quicklisp-functions

;; [[file:step.org::quicklisp-functions][quicklisp-functions]]
;; (load "/home/quill/quicklisp/setup.lisp")
    ;; (ql:quickload :computable-reals)
    ;; (use-package :computable-reals)
;; quicklisp-functions ends here

;; math-functions

;; #+name: math-functions

;; [[file:step.org::math-functions][math-functions]]
(defconstant *pi* 3.141592653589793d0)
  (defconstant *2pi* 6.283185307179586d0)

  (defun rad-to-deg (a)
    (* (/ 180.0d0 *pi*) a))

  (defun deg-to-rad (a)
    (/ a (/ 180.0d0 *pi*)))

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

  (defun vector-scale (v scale-factor)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (length (length-vector v))
           (angle (angle-vector v)))
      (list p1 (list (+ p1x (* length (cos angle))) (+ p1y (* length (sin angle)))))
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

  (defun vector-transform-by-vector (v tr-vector)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
           (p1x (x-of p1))
           (p1y (y-of p1))
           (p2x (x-of p2))
           (p2y (y-of p2))
           (tr-point (list (- p2x p1x) (- p2y p1y))))
      (list (p+ p1 tr-point) (p+ p2 tr-point))
      ))

  (defun vector-offset (v distance)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v))
	   (vrotate (vector-rotate v (* *pi* 0.5)))
	   (normal-vrotate (normal-vector vrotate))
	   (dvector (vector-scale normal-vrotate distance))
	   )
      (vector-transform-by-vector v dvector)
      ))

  (defun vector-reverse (v)
    (let* ((p1 (nth 0 v))
           (p2 (nth 1 v)))
      (list p2 p1)
      ))
;; math-functions ends here

;; stream-patches
;; #+name: stream-patches

;; [[file:step.org::stream-patches][stream-patches]]
(defun prologue (spindle output-stream)
    (format output-stream "(prologue)~% M3~% M5~% M9~% S~d ~% ~% (end of prologue)~%" spindle)
    )
  (defun epilogue (output-stream)
    (format output-stream "~%  (epilogue) M3~% M5~% M30~% ~% %% ~% (end of program)~% %%")
    )
;; stream-patches ends here

;; point-functions

;; #+NAME: point-functions

;; [[file:step.org::point-functions][point-functions]]
(defun x-of (point) (nth 0 point))
    (defun y-of (point) (nth 1 point))
    (defun z-of (point) (nth 2 point))

  (defun add-vector-point (point-1 point-2)
    (mapcar #'+ point-1 point-2))
;; point-functions ends here

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
      (format str "G02 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

  (defun counter-clockwise-move-R (point r f str)
    (format str "G03 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))

  (defun ccw-move-R (point r f str)
    (format str "G03 ~4T X~8,3F ~15T Y~8,3F ~25T Z~8,3F  R~8,3F  F~D ~%" (x-of point) (y-of point) (z-of point) r f))
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
                     ))))
      )

(defun drill-array (point-array z-list f- f+ output-stream)
    (loop for point in point-array
          do (drill-point point z-list f- f+ output-stream)
          ))

;;;linear move
    (setq l-step 1.5d0)
          ;;;radius of circle
    (setq radius 1.5d0)
    (setq zi 0.0)
    (setq f- 600)

    (setq start-point '(0.0d0 0.0d0))

    (setq output-stream *STANDARD-OUTPUT*)

    (setq x0 (x-of start-point))
    (setq y0 (y-of start-point))

    (prologue 10000 output-stream)
    (dotimes (n 100)
      (let ((xi (+ x0 (* n l-step)))
            (yi (+ y0 (* n l-step))))
        (linear-move (list xi y0 zi)  f- output-stream)
        (clockwise-move-R (list xi (+ y0 radius radius) zi) radius f- output-stream)
        (clockwise-move-R (list xi (+ y0 0.0) zi) radius f- output-stream)
        ))
  (epilogue output-stream)

(defun point-couples (center radius trochoidal-width no-of-points)
    (let* ((internal-points (divide-circle center radius no-of-points))
           (external-points (divide-circle center (+ radius trochoidal-width) no-of-points))
           (couples (mapcar #'list internal-points external-points)))
      couples))

(setq point1 '(0.0 0.0))
(setq point2 '(100 0.0))
(setq point3 '(100 100.0))
(setq point4 '(0 100.0))

(setq trochoidal-width 10.0)
(setq l-step 9.1)

(setq vector1 (list point1 point2))
(setq vector2 (list point2 point3))
(setq vector3 (list point3 point4))
(setq vector4 (list point4 point1))

(setq length-vector1 (length-vector vector1))
(setq direction-vector1 (length-vector vector1))

(setq no-of-points (round(/ length-vector1 l-step)))
(setq r-step (/ length-vector1 no-of-points))

(defun divide-vector (v step-length)
  (let* ((vdir (normal-vector v))
         (vdir-per (normal-vector-rotate vdir (* *pi* 05d0)))
         (start-point (nth 0 v))
         (end-point (nth 1 v))
         (vlength (length-vector v))
         (no-of-points (round(/ length-vector1 l-step)))
         (r-step (round(/ vlength no-of-points)))
         (vector-array '()))
    (progn
      (dotimes (i no-of-points)
        (let ((point (p+ start-point (scale-point vdir (* i r-step))))
              )
          (push point vector-array)
          ))
      (reverse vector-array))))

(defun offset-vector (v offset)
  (let* ((vdir (normal-vector v))
         (start-point (nth 0 v))
         (end-point (nth 1 v))
         (vlength (length-vector v))
         (vector-array '())
         (vdir-perpendicular (normal-vector-rotate vdir (* *pi* 0.5)))
         (new-start-point (p+ start-point (scale-point vdir-perpendicular offset)))
         (new-end-point (p+ end-point (scale-point vdir-perpendicular (sqrt offset)))))
    (list new-start-point new-end-point)))

(setq v vector2)
(setq offset 10.0d0)

(setq vdir (normal-vector v))
(setq start-point (nth 0 v))
(setq end-point (nth 1 v))
(setq vlength (length-vector v))
(setq vector-array '())
(setq vdir-perpendicular (vector-rotate vdir (* *pi* 0.5)))
;;(setq new-start-point (p+ start-point (vector-scale vdir-perpendicular offset)))
;;(setq new-end-point (p+ end-point (scale-point vdir-perpendicular (sqrt offset))))


;; (let ((v start-point))
;;   (format t "| start-point: |  ~8,3F | ~8,3F | ~%" (x-of v) (y-of v)))
;; (let ((v end-point))
;;   (format t "| end-point: |  ~8,3F | ~8,3F | ~%" (x-of v) (y-of v)))
;; (let ((v vdir))
;;   (format t "| vdir: |  ~8,3F | ~8,3F | ~%" (x-of v) (y-of v)))
;; (let ((v vlength))
;;   (format t "| vlength: |  ~8,3F | | ~%" v))
;; (let ((v (vector-rotate vector4 (* *pi* 0.5))))
;;   (format t "| vdir-perpendicular: |  ~8,3F | ~8,3F | ~%" (x-of v) (y-of v)))

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
;; trochoidal-circle ends here

;; test-trochoidal-circle-external-610
  
;; #+NAME: test-trochoidal-circle-610

;; [[file:step.org::test-trochoidal-circle-610][test-trochoidal-circle-610]]
(setq output-stream *STANDARD-OUTPUT*)
                    ;;;linear step
  (setq l-step 10.0d0)
  (setq zi 0.0)
  (setq f- 900)
  (setq center '(0.0 0.0))
  (setq tool-diameter 3.175d0)
  (setq internal-radius (/ 610.0 2.0d0))
                          ;;;most internal radius of the path

  (setq trochoidal-width 4.5)
  (setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))

  (external-trochoidal-circle l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; test-trochoidal-circle-610 ends here

;; test-trochoidal-circle-internal-500
  
;; #+NAME: test-trochoidal-internal-circle-500

;; [[file:step.org::test-trochoidal-internal-circle-500][test-trochoidal-internal-circle-500]]
(setq output-stream *STANDARD-OUTPUT*)
  (setq l-step 2.0d0)
  (setq zi 7.5)
  (setq f- 900)
  (setq center '(0.0 0.0))

  (setq internal-radius (/ 498 2.0d0))
  (setq tool-diameter 3.175)
  (setq trochoidal-width 4.5)
  (setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))

  (internal-trochoidal-circle l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; test-trochoidal-internal-circle-500 ends here

;; test-trochoidal-circle-internal-496
  
;; #+NAME: test-trochoidal-internal-circle-496

;; [[file:step.org::test-trochoidal-internal-circle-496][test-trochoidal-internal-circle-496]]
(setq output-stream *STANDARD-OUTPUT*)
                    ;;;linear step
(setq l-step 1.5d0)
(setq zi 0.0)
(setq f- 900)
(setq center '(0.0 0.0))
(setq internal-radius (/ 494 2.0d0))
(setq tool-diameter 3.175)
(setq trochoidal-width 4.0)

(internal-trochoidal-circle l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; test-trochoidal-internal-circle-496 ends here
