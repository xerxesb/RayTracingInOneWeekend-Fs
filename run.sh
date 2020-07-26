#!/bin/bash
#
# Runs the app and dumps output to the test file. Then opens the file in the QuickLook viewer
#
# Arg 1 should be the name of the image you want to generate.
# Convention is to make it the same as the chapter number 

if [ -z "$1" ]
  then
    echo "No argument supplied. Provide the exercise number (e.g. 3.2) and it will spit out a file 3.2.ppm"
    exit 1
fi

dotnet run > test_image/$@.ppm && (test_image/qlf test_image/$@.ppm &)
