

#1=0.000 (x-of helical hole)
#2=0.000 (y-of helical hole)
#3=900.0 (helical feed rate)

#4=0.000 (x-of horizontal line)
#5=0.000 (y-of horizontal line)
#6=15.00 (step of horizontal line)


O40 (run vertical lines)
#2=0
M98P30L10
#2=0
M99

O30 (vertical line)
M98P20L01
#2=[#2+#6]
M99

O20 (run horizontal lines)
#1=0.000
M98P10L10
#1=0.000
M99

O10 (horizontal line)
M98P100L01
#1=[#1+#6]
M99

O100 (helical drilling point)
G0    X [#1+3.412]   Y [#2]   Z   8.000   
G02   X [#1-3.412]   Y [#2]   Z   4.000  R   3.412  F[#3] 
G02   X [#1+3.412]   Y [#2]   Z   0.000  R   3.412  F[#3]  
G02   X [#1-3.412]   Y [#2]   Z   0.000  R   3.412  F[#3]  
G02   X [#1-3.412]   Y [#2]   Z   4.000  R   3.412  F[#3]  
G02   X [#1+3.412]   Y [#2]   Z   8.000  R   3.412  F[#3]  
M99

O200 (normal drilling point)
G00   X [#1]   Y [#2]   Z 8.000   
G00   X [#1]   Y [#2]   Z 5.000
G01   X [#1]   Y [#2]   Z 0.000 F[#3]  
G00   Z 8.000
M99



M30

%
