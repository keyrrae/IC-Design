#! /apps/ame/bin/python

import os
import re

stepsOfSAR = 8

for i in xrange(stepsOfSAR):
	#send command to shell
	os.system("spectre netlist" + str(i) +".scs")
	
	#read in simulation result
	with open("trimmedVal" + str(i) + "txt") as fReslt:
		midResltVal = float(fReslt.read()[0].strip())

	#implement SAR algorithm here
	dstTrmVal = 0.0


	#write the target to a new .scs file

	with open("netlist0.scs",'r') as orgNetlst:
		if i < 7 :
			with open("netlist" + str(i+1) + ".scs", 'r') as dstNetlst:
				for line in orgNetlst:
					if re.search("p_trim", line):
						dstNetlst.write(""+ str(dest)+"\n")
					elif re.search('trimmedVal0.txt', line):
						dstNetlst.write("\t\t\tfbits = $fopen(\"~/trimmedVal" + str(i) +".txt\");\n"
					else:
						dstNetlst.write(line)
with open("trimmedVal" + str(stepsOfSAR-1) +".scs",'r') as fFnlReslt:
	print "Finished. The value of ... is" + str(fReslt.read()[0].strip())
#os.system("rm ")
