






(assert (equal (included-angle -90 0 'cw 360.0d0) 270.0d0))
(assert (equal (included-angle 0 -90 'cw 360.0d0) 90.0d0))

(assert (equal (included-angle 270 0 'ccw 360.0d0) 90.0d0))
(assert (equal (included-angle 0 270 'ccw 360.0d0) 270.0d0))


;;test divide arc
(divide-arc '(0.0d0 0.0d0) 1.0d0 (/ *pi* 2.0) 0 'ccw 4)



