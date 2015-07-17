#! /apps/ame/bin/python

import os
import re

stepsOfSAR = 8 
refVal = 1.0
nxtTrmVal = 0.5
trgtVal = 0.712151
paramName = "p_trim"
netlstTempName = "netlistTmplt.scs"
ahdlName = "saveResult"
trimmedResltFName = "trimmedVal.txt"
finalResltName = "final.txt"
resltArray = []

for i in xrange(stepsOfSAR):

	# write current trim value to a new .scs file
	# prepare spectre input file
	with open(netlstTempName,'r') as orgNetlst:
		with open("netlist" + str(i) + ".scs", 'w') as dstNetlst:
			for line in orgNetlst:
				if re.search(paramName, line) and re.search("parameters", line):
					dstNetlst.write("parameters " + paramName + '=' + str(nxtTrmVal)+'\n')

				elif re.search(ahdlName, line) :
					if re.search("ahdl_include",line):
						dstNetlst.write("ahdl_include \"./" + ahdlName + str(i) + ".va\"\n")
					else:
						dstNetlst.write("I0 (vo) " + ahdlName + str(i) + "\n")

				else:
					dstNetlst.write(line)

	with open(ahdlName + ".va", 'r') as ahdlTemplt:
		with open(ahdlName + str(i) +".va",'w') as ahdlFile:
			for line in ahdlTemplt:
				if re.search("trimmedVal.txt", line):
					ahdlFile.write("\t\t\tfbits = $fopen(\"./trimmedVal" + str(i) +".txt\");\n")
				elif re.search("module",line) and re.search(ahdlName,line):
					ahdlFile.write(re.sub(ahdlName, ahdlName + str(i), line))
				else:	
					ahdlFile.write(line)

	# send command to shell
	os.system("spectre netlist" + str(i) +".scs")

	# read in simulation result
	with open("trimmedVal" + str(i) + ".txt",'r') as fReslt:
		midResltVal = float(fReslt.read().strip())
	
	resltArray.append(str(midResltVal)+"\n")

	# implement algorithm here
	if midResltVal > trgtVal : 
		nxtTrmVal = nxtTrmVal - refVal / (2 ** (i+2))
	elif midResltVal < trgtVal : 
		nxtTrmVal = nxtTrmVal + refVal / (2 ** (i+2))

with open(finalResltName, 'w') as ffnl:
	for i in xrange(len(resltArray)):
		ffnl.write(resltArray[i])

print "===============Trim Log==============="
os.system("cat " + finalResltName )

