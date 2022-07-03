

(shadow '+)

(defgeneric + (a &rest b))

(defmethod + ((a number) &rest b) (apply 'cl:+ a b))
(+ 1 2)
(+ 2 3 4)


(defmethod + ((a string) &rest b) (apply #'cl:concatenate 'string a b))
(+ "Hello" "World")
"HelloWorld"
(+ "Hello" " cruel " "World")
"Hello cruel World"

(defmethod + ((a vector) &rest b) (apply #'map 'vector 'cl:+ a b))
(let ((v0 #(1 2 3)) (v1 #(4 5 6))) (+ v0 v1))

(defmethod + ((a list) &rest b) (apply #'map 'list 'cl:+ a b))
(let ((v0 #(1 2 3)) (v1 #(4 5 6))) (+ v0 v1))




(defpackage generic-arithmetic 
  (:use "COMMON-LISP")
  (:shadow "+"))

(in-package generic-arithmetic)

(setf *pi* 3.14159)


(defun + (&rest addends)
  (reduce 'binary+ (cdr addends) :initial-value (car addends)))

(defgeneric binary+ (addend1 addend2))

(defmethod binary+ ((x number) (y number))
  (cl:+ x y))

(defmethod binary+ ((x vector) (y vector))
  (map 'vector 'cl:+ x y))

(defmethod binary+ ((x list) (y list))
  (map 'list 'cl:+ x y))

(use-package 'generic-arithmetic)

(+ '(1 1.0) '(2 2))


(defclass polygon ()
  (
   (outside_radius :accessor outside_radius)
   (no_of_sides :accessor no_of_sides)
   (start_angle :accessor start_angle)
  	  )
  )

(setf item (make-instance 'polygon))


(defun polygon_angle_rad (no_of_sides)
  "`polygon_angle_rad' returns the center angle
`no_of_sides' number of sides
"
  (/ (* 2 *pi*) no_of_sides)
  )

(defun polygon_angle_deg (no_of_sides)
  (/ 360.0d0 no_of_sides))



(assert (< (- (polygon_angle_rad 4.0d0) (/ *pi* 2.0)) 1e-9))
(assert (< (- (polygon_angle_deg 4) 90.0d0) 1.0))

(defun get-points-of-polygon (center outside_radius no_of_sides)
  (let ((angle (polygon_angle_rad no_of_sides))
	(Xc (x-of center))
	(Yc (y-of center))
	(Zc (z-of center))
	)
    (loop for i from 1 upto no_of_sides
	  collect (list
		   (+ Xc (* outside_radius (cos (* i angle))))
		   (+ Yc (* outside_radius (sin (* i angle))))
		   Zc))))


(defun gcode_polygon (center outside_radius no_of_sides)
  (let ((point_list
	 (get-points-of-polygon center outside_radius no_of_sides)
	  ))
    (loop for point in point_list
	  do (linear-move point 900.0 *STANDARD-OUTPUT*))
    ))

  

(gcode_polygon '(0.0d0 0.0d0 0.0d0) 1.0 4)

		   


 
