;; Trochoidal-circle-external-array-610
  
;; #+NAME: trochoidal-circle-array-610-data
;; #+HEADER: :tangle yes :noweb yes
;; #+HEADER: :results output 
;; #+HEADER: :comments both :cache yes :session step

;; [[file:run.org::trochoidal-circle-array-610-data][trochoidal-circle-array-610-data]]
(setq output-stream *STANDARD-OUTPUT*)
(setq l-step 20.0d0)
(setq zi 0.0)
(setq f- 900)
(setq center-array (list '(0.0 0.0) '(100 0.0) '(100.0 100.0) '(100.0 0.0)))
(setq center '(0.0 0.0))
(setq tool-diameter 50d0)
(setq internal-radius (/ 100.0 2.0d0))
(setq trochoidal-width 40)
(setq external-radius (+ internal-radius trochoidal-width))
(setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))
;; trochoidal-circle-array-610-data ends here



;; #+RESULTS[4c92bf8419c841ab74c60eaf4f85ed0616867e47]: trochoidal-circle-array-610-data


;; #+NAME: trochoidal-circle-array-610-print
;; #+HEADER: :tangle yes :noweb yes
;; #+HEADER: :results output raw 
;; #+HEADER: :comments both :cache yes :session step

;; [[file:run.org::trochoidal-circle-array-610-print][trochoidal-circle-array-610-print]]
(format t "|l-step|~f| | mm|~%" l-step)
(format t "|zi |~f|  |mm|~%" zi)
(format t "|f- |~f| | mm/min|~%" f-)
(format t "|----|~%")
(loop for center in center-array
      do (format t "|center:|~f|~f |mm x mm|~%" (car center) (cadr center)))
(format t "|----|~%")
(format t "|tool-diameter |~f| | mm|~%" tool-diameter)
(format t "|trochoidal-width|~f|| mm| ~%" trochoidal-width)
(format t "|internal-radius|~f| | mm |~%" internal-radius)
(format t "|external-radius|~f| | mm|~%" external-radius)
(format t "|----|~%")
(format t "|internal-cutting-radius|~f| | mm |~%" (- internal-radius (/ tool-diameter 2.0)))
(format t "|external-cutting-radius|~f| | mm|~%" (+ external-radius (/ tool-diameter 2.0)))
(format t "|internal-cutting-daimeter|~f| | mm |~%" (* 2.0 (- internal-radius (/ tool-diameter 2.0))))
(format t "|external-cutting-diameter|~f| | mm|~%" (* 2.0(+ external-radius (/ tool-diameter 2.0))))
(format t "|----|~%")
;; trochoidal-circle-array-610-print ends here



;; #+RESULTS[3372bd16d8c879731f6cb246e2c4494f0d81e22b]: trochoidal-circle-array-610-print
;; | l-step                    |  20.0 |       | mm      |
;; | zi                        |   0.0 |       | mm      |
;; | f-                        | 900.0 |       | mm/min  |
;; |---------------------------+-------+-------+---------|
;; | center:                   |   0.0 |   0.0 | mm x mm |
;; | center:                   | 100.0 |   0.0 | mm x mm |
;; | center:                   | 100.0 | 100.0 | mm x mm |
;; | center:                   | 100.0 |   0.0 | mm x mm |
;; |---------------------------+-------+-------+---------|
;; | tool-diameter             |  50.0 |       | mm      |
;; | trochoidal-width          |  40.0 |       | mm      |
;; | internal-radius           |  50.0 |       | mm      |
;; | external-radius           |  90.0 |       | mm      |
;; |---------------------------+-------+-------+---------|
;; | internal-cutting-radius   |  25.0 |       | mm      |
;; | external-cutting-radius   | 115.0 |       | mm      |
;; | internal-cutting-daimeter |  50.0 |       | mm      |
;; | external-cutting-diameter | 230.0 |       | mm      |
;; |---------------------------+-------+-------+---------|

;; #+NAME: trochoidal-circle-array-610-gcode
;; #+HEADER: :tangle yes :noweb yes
;; #+HEADER: :results output  file :file trochoidal-circle-610-diameter-internal.ngc
;; #+HEADER: :comments both :cache yes :session step

;; [[file:run.org::trochoidal-circle-array-610-gcode][trochoidal-circle-array-610-gcode]]
(setq output-stream *STANDARD-OUTPUT*)
(setq l-step 20.0d0)
(setq zi 0.0)
(setq f- 900)
(setq center-array (list '(0.0 0.0) '(100 0.0) '(100.0 100.0) '(100.0 0.0)))
(setq center '(0.0 0.0))
(setq tool-diameter 50d0)
(setq internal-radius (/ 100.0 2.0d0))
(setq trochoidal-width 40)
(setq external-radius (+ internal-radius trochoidal-width))
(setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))
(external-trochoidal-circle-array l-step zi f- center-array tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; trochoidal-circle-array-610-gcode ends here

;; Trochoidal-circle-internal-500

;; #+NAME: test-trochoidal-internal-circle-500

;; [[file:run.org::test-trochoidal-internal-circle-500][test-trochoidal-internal-circle-500]]
(setq output-stream *STANDARD-OUTPUT*)
(setq l-step 2.0d0)
(setq zi 0.0)
(setq f- 900)
(setq center '(0.0 0.0))

(setq internal-radius (/ 496 2.0d0))
(setq tool-diameter 3.175)
(setq trochoidal-width 4.5)
(setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))

(internal-trochoidal-circle l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; test-trochoidal-internal-circle-500 ends here

;; Trochoidal-circle-internal-496

;; #+NAME: test-trochoidal-internal-circle-496

;; [[file:run.org::test-trochoidal-internal-circle-496][test-trochoidal-internal-circle-496]]
(setq output-stream *STANDARD-OUTPUT*)
(setq l-step 2.0d0)
(setq zi 0.0)
(setq f- 900)
(setq center '(0.0 0.0))

(setq internal-radius (/ 495 2.0d0))
(setq tool-diameter 3.175)
(setq trochoidal-width 4.5)
(setq z-list-in '(50.0 18.0 16.0 15.0 12.0 8.0 5.0 0.0 0.0))

(internal-trochoidal-circle l-step zi f- center tool-diameter internal-radius trochoidal-width z-list-in output-stream)
;; test-trochoidal-internal-circle-496 ends here

(setq output-stream (open "/home/quill/test-D.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
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

(prologue 8000 output-stream)
(external-D-trochoidal-circle
 xystep zstep zsafe zstart zend fz fxy center tool-diameter internal-diameter external-diameter output-stream 1.0d0)
(epilogue output-stream) 

(close output-stream)

(setq output-stream (open "/home/me/t.ngc" :direction :output :if-does-not-exist :create :if-exists :supersede))
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
