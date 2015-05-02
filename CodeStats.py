import os
import time
import collections

filenames = []
for root, dirs, files in os.walk(".."):
    for name in files:
        filenames.append(str(os.path.join(root, name)))


keys = ['py', 'go', 'c', 'cpp', 'java', 'r', 'm', 'hs', 'scala']
progName = ['Python :\t', 'Go :\t', 'C :\t', 'C++ :\t', 'Java :\t',
            'R :\t', 'Matlab :\t', 'Haskell :\t', 'Scala :\t']
dic1 = {key: 0 for key in keys}
dic2 = {keys[i]: progName[i] for i in xrange(len(keys))}
#print dic

now = time.strftime("%c")
with open("../CodeStat.log", 'a') as log:
    log.write(now+'\n')

    for fileIns in filenames:
        count = 0

        with open(fileIns, 'r') as f:
            for line in f:
                count += 1
        fileNameFields = fileIns.strip().split('\\')
        filetype = fileNameFields[-1].split('.')
        filetype = filetype[-1].lower()

        if filetype in keys:
            dic1[filetype] += count

    odic1 = collections.OrderedDict(sorted(dic1.items()))
    total = 0
    for key, value in odic1.iteritems():
        log.write(str(dic2[key]) + str(value)+'\n')
        total += value
    log.write('Total : ' + str(total) + '\n\n')
