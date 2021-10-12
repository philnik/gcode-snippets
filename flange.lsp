


(setq D1 1000.0)
(setq D2 600.0)
(setq D3 590.0)

(setq R1 400.0)

(defun c:loop2 () 
  	(command "._layer" "set" "0" " ")
	(setq center '( 0 0 0))
	(command "circle" center "DIAM" D1)
	(setq count 0)
	(while (< count 10)
	  (setq angle (* (/ 3.14159 180.0) (* count 36)))
	  (setq pnt1 (list (* R1 (cos angle)) (* R1 (sin angle))))
	  (command "circle" pnt1 "DIAM" 100.0)
	  (setq count (+ 1 count))
	  )

;	(command "._layer" "set" "P" " ")
	(command "circle" center "DIAM" D2)
	(command "circle" center "DIAM" D3)

)
