import subprocess
import re
import sys

if __name__== '__main__':
    compilerLoc = sys.argv[1]
    if compilerLoc == "-h" or compilerLoc == "--help":
        print("format: [python] refCompileScript.py [compilerLocation] [fileLocation]\n")
        sys.exit()
    fileLoc = sys.argv[2]
    fileNameLong = re.findall(r'(.+).wacc',fileLoc)[0]
    fileNameShort = re.findall(r'\/([^\/]+)\.wacc',fileLoc)[0]
    fileNameShortRef = re.findall(r'\/([^\/]+)\.wacc',fileLoc)[0]+"_ref"
    print(fileNameShortRef)
    print(compilerLoc)
    return_code = subprocess.call("./{compilerLoc} {fileLoc} -a > temp.txt"
                                  .format(compilerLoc=compilerLoc, fileLoc=fileLoc), shell=True)
    print(return_code)
    r = r'\d+\t(.+)'
    with open('temp.txt', 'r') as f, open(fileNameShortRef+".s", "w+") as fw:
        for line in f.readlines():
            o = re.findall(r, line)
            if o:
              print(o[0])
              fw.write(o[0]+"\n")
#     subprocess.call("open " + fileLoc, shell=True)
#     subprocess.call("open " + fileNameShortRef+".s", shell=True)