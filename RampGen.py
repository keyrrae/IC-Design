import string

framp=open('ramp.txt','w')
for i in xrange(2**18):
    framp.write(str(i)+'\n')

framp.close()
