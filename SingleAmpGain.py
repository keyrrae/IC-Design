import math


VCC = 3.0
R1 = 50.799
R2 = 50.849e3
gain = 1.0 + R2 / R1
#print "gain:"+str(gain)
Vo1 = 0.266
V1 = 0.112
Vo2 = 0.325
V2 = 0.115
Aol = 20.0 * math.log10((Vo2 - Vo1)/(V2/gain - V1/gain))
print Aol
