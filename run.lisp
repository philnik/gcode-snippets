(setq output-stream (open "/home/me/lispcnc/test-simple-drill-on-circle.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq f- 600.0)
(setq zsafe 30.0)
(setq zstart 12.0)
(setq zend 0.0)

(setq output-stream *STANDARD-OUTPUT*)

(setq center '(0.0d0 0.0d0))
(setq divide-radius 100.0)
(setq no-of-points 4) 
(setq point-array (divide-circle center divide-radius no-of-points))

(prologue 2500 output-stream) 
(drill-point-array point-array zsafe zstart zend f- f- output-stream)
(epilogue output-stream)

(close output-stream)

(setq output-stream (open "/home/me/lispcnc/test-helical-drill-on-circle.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq zsafe 25.0)
(setq zstart 15.0)
(setq zend 0.0)
(setq zstep 2.0)

(setq z-list (helical-z-list zsafe zstart zend zstep))

(setq hole-diameter 10.0d0)
(setq tool-diameter 3.175)
(setq path-radius (/ (- hole-diameter tool-diameter) 2.0))

(setq f- 1600.0)
(setq center '(0.0d0 0.0d0))
(setq divide-radius 500.0)
(setq no-of-points 20) 
(setq point-array (divide-circle center divide-radius no-of-points))
(prologue 12000 output-stream) 
(helical-drill-array point-array zsafe zstart zend zstep path-radius f- output-stream)
(epilogue output-stream)

(close output-stream)

(let ((point-array (quote ((47.0 47.0) (213.66 47.0) (380.33 47.0) (547.0 47.0) (547.0 213.66) (547.0 213.66) (547.0 380.33) (547.0 547.0) (380.33 547.0) (213.66 547.0) (47.0 547.0)))))
(setq output-stream (open "/home/me/lispcnc/test-helical-drill-on-array.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq zsafe 25.0)
(setq zstart 15.0)
(setq zend 0.0)
(setq zstep 2.0)

(setq hole-diameter 10.0d0)
(setq tool-diameter 3.175)
(setq path-radius (/ (- hole-diameter tool-diameter) 2.0))
(setq f- 600.0)

(prologue 3000 output-stream) 
(helical-drill-array point-array zsafe zstart zend zstep  path-radius f- output-stream)
(epilogue output-stream)
(close output-stream)
)

(setq output-stream (open "/home/me/lispcnc/test-D.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq xystep 3.00d0)
(setq zstep 4.0)
(setq zsafe 50.0)
(setq zstart 12.0)
(setq zend 0.0)
(setq fz 2000)
(setq fxy 2000)
(setq center '(0.0 0.0))
(setq tool-diameter 3.175)
(setq internal-diameter 580.0)
(setq external-diameter 596.0)

(trochoidal-data-gcode-prologue
 xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)

(external-D-trochoidal-circle
 xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream 1.0d0)
(epilogue output-stream) 

(close output-stream)

(setq output-stream (open "/home/me/lispcnc/test-C.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq xystep 9.00d0)
(setq zstep 3.0)
(setq zsafe 50.0)
(setq zstart 12.0)
(setq zend 0.0)
(setq fz 2000)
(setq fxy 2000)
(setq center '(0.0 0.0))
(setq tool-diameter 3.175)
(setq internal-diameter 560.0)
(setq external-diameter 596.0)

(trochoidal-data-gcode-prologue
 xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)

(external-trochoidal-circle
 xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream)
(epilogue output-stream) 

(close output-stream)

(setq output-stream (open "/home/me/lispcnc/line.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
(setq xystep 3.00d0)
(setq zstep 4.0)
(setq zsafe 50.0)
(setq zstart 12.0)
(setq zend 0.0)
(setq fz 2000)
(setq fxy 2000)
(setq tool-diameter 3.175)
(setq trochoidal-width 80.0)
(setq v (list '(0.0d0 0.0d0) '(100.0d0 0.0d0)))

(prologue 8000 output-stream)

(external-trochoidal-line xystep zstep zsafe zstart zend fz fxy tool-diameter v trochoidal-width output-stream)

(epilogue output-stream) 

(close output-stream)
