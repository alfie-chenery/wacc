import subprocess
import re
import sys
import time


def fail():
  with open("out.txt","w+") as f:
    f.write("failed")




if __name__== '__main__':
    compilerLoc = sys.argv[1]
    if compilerLoc == "-h" or compilerLoc == "--help":
        print("format: [python] runTest.py [compilerLocation] [testfileLocation]\n")
        sys.exit()
    fileLoc = sys.argv[2]
    fileNameLong = ""
    try:
      fileNameLong = re.findall(r'(.+).wacc',fileLoc)[0]
    except e:
      fail()
    fileNameShort = ""
    try:
      fileNameShort = re.findall(r'\/([^\/]+)\.wacc',fileLoc)[0]
    except:
      fail()
    fileNameShortRef = ""
    try:
      fileNameShortRef = re.findall(r'\/([^\/]+)\.wacc',fileLoc)[0]+"_ref"
    except:
      fail()
    subprocess.run(["python3", "refCompileScript.py", "./"+compilerLoc, fileLoc])
    # now we have a file called print.s that has the reference assembly
    subprocess.run(["make"])
    subprocess.run(["./compile", fileLoc])
    # now we ALSO have a print.s file that has our implementation's assembly in it

    from subprocess import PIPE
    input = "one\n"
    exitCodeRegex = r'The exit code is: (\d)\.'
    emulationOutputRegex = r'-- Emulation Output:\n([^(\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-\-)]+)\n---------------------------------------------------------------'

    p1 = subprocess.run(["./refEmulate", fileNameShort+".s"], stdout=PIPE,
            input=input, encoding='ascii')
    out1 = p1.stdout
    exitCode1 = 0
    if re.findall(exitCodeRegex,out1):
#       print(re.findall(exitCodeRegex,out1))
      exitCode1 = re.findall(exitCodeRegex,out1)[0]
    else:
      exitCode1 = 123
#     emulationOutput1 = re.findall(emulationOutputRegex,out1)[0]
#     print(exitCode1)
#     print(emulationOutput1)

#     subprocess.run(["echo",input])
#     print(p2.returncode)
    p2 = subprocess.run(["./refEmulate", fileNameShortRef+".s"], stdout=PIPE,
            input=input, encoding='ascii')
    out2 = p2.stdout
#     exitCode2 = re.findall(exitCodeRegex,out2)[0]
    exitCode2 = 0
    if re.findall(exitCodeRegex,out2):
      exitCode2 = re.findall(exitCodeRegex,out2)[0]
    else:
      exitCode2 = 123
#     emulationOutput2 = re.findall(emulationOutputRegex,out2)[0]
#     print(exitCode2)
#     print(emulationOutput2)
    with open("out.txt","w+") as f:
      if (exitCode1 == exitCode2 and exitCode1==exitCode2):
        f.write("success")
      else:
        f.write("failed")


