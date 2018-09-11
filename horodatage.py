import os
import datetime

pre = "Car340053-2018-Pass2-A2-0_"
suff = "_000"

for path, dirs, files in os.walk("wav/"):
    for file in files:
        #print(path+file)
        filename, file_extension = os.path.splitext(file)
        #print(filename)
        #print(file_extension)
        #print(type(filename))
        unixdate = int(filename[:8], 16)
        #print(unixdate)
        maDate = datetime.datetime.fromtimestamp(unixdate).strftime('%Y%m%d_%H%M%S')
        #print(str(maDate))
        newFile = pre + maDate + suff
        print(newFile+file_extension)
        os.rename(os.path.join(path, file), os.path.join(path, newFile+file_extension))
