
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

(defclass arc ()
  (
   (name
    :initarg :name
    :accessor name
    :initform "anononymous")
   (center
    :initarg :center
    :accessor center
    :documentation "center: list of 3 floats"
    :initform '(0.0 0.0 0.0))
   (direction
    :initarg :direction
    :accessor direction
    :documentation "direction: list of 3 values"
    :initform '(0.0 0.0 1.0))
   (radius
    :initarg :radius
    :documentation "radius: 1 float"
    :accessor radius
    :initform 0.0)
   (uv-params
    :initarg :uv-params
    :documentation "uv-params: list of 2 values"
    :accessor uv-params
    :initform '(0.0 6.28))
   ))

(setq arc1 (make-instance 'arc
			  :name "my-arc"
			  :center '(0.0 0.0 0.0)
			  :radius 1.00
			  :direction '(0 0 1)
			  :uv-params '(0 1.5)))

(center arc1)
(direction arc1)
(uv-params arc1)
(radius arc1)


(defmethod return-type-of-object (obj)
  (format t "type of ~a~%" (type-of obj)))

(defmethod arc-from-three-points ((myarc arc) start-point mid-point end-point)
  "arc-from-points: creates an arc object from 3 points
`start-point' start-point from arc
`mid-point' mid point from arc
`end-point' end point of arc
"
;;; We need first to find the plane of the 3 points and then work on the 2d arc
  )

(defmethod symmetric-arc-from-center-radius-angle ((myarc arc) center radius angle)
  "symmetric-arc-from-center-radius-angle: returns a symmetric arc from variables
`center' center point of the arc ( x y)
`radius' radius of the arc
`angle' total angle
"
  (setf (center myarc) center)
  (setf (radius myarc) radius)
  )


(defmethod rotate-arc ((myarc arc) center angle)
  "rotates an arc around center by angle
`center' center of arc
`angle' angle of rotation
"
  )

(defmethod copy-arc ((myarc arc) center angle)
  "rotates an arc around center by angle
`center' center of arc
`angle' angle of rotation
"
  )


;(second (closer-mop:class-slots (find-class 'arc)))


(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
            (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;(copy-instance arc1)

(defun shallow-copy-object (original)
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name (closer-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(shallow-copy-object arc1)




(defmethod print-arc ((obj arc) out)
  (with-slots (name center direction radius uv-params) obj
    (print-unreadable-object (obj out :type t)
      (format out "~% name:~A~% center:~A~% direction:~A~% radius:~A~% uv-params:~A~%" name center direction radius uv-params))))


(symmetric-arc-from-center-radius-angle arc1 '(1 1 1) 20.0 30.0)
(print-arc arc1 *STANDARD-OUTPUT*)

(defun set-uv-params-from-center-angle (center-angle arc-angle)

  )

(defun set-center-from-radius-and-angle (radius center-angle)
  (list
   (* radius (cos center-angle))
   (* radius (sin center-angle))))

(defun make-arc (name &key center radius direction uv-params)
  (make-instance 'arc
   :name name
   :center center
   :radius radius
   :direction direction
   :uv-params uv-params))

(setq s "hello")
(setq arc3 (make-arc "hello"))

(center arc3)

(with-slots (center radius)
    arc1
  (format t "~s --- ~s ~%" center radius))

(inspect arc1)
