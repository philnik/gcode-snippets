






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

		   

 
