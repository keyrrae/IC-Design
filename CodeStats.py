import os
import time


#d = os.system("pwd")
#print d
filenames = []
for root, dirs, files in os.walk(".."):
    for name in files:
        filenames.append(str(os.path.join(root, name)))
    #for name in dirs:
    #    print(os.path.join(root, name))

keys = ['py', 'go', 'java', 'r', 'm', 'hs']
progName = ['Python :\t', 'Go :\t', 'Java :\t', 'R :\t', 'Matlab :\t',
            'Haskell :\t']
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
        #print filetype
        #print fileNameFields
        #os.system('cat '+fileIns+'| wc -l 2>./blackhole')
    total = 0
    for key in dic1:
        log.write(str(dic2[key]) + str(dic1[key])+'\n')
        total += dic1[key]
    log.write('Total : ' + str(total) + '\n\n')
#   f.write(os.system("`find . -name '*.py' | xargs wc -l`"))
