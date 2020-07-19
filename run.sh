#!/bin/bash
#
# Runs the app and dumps output to the test file. Then opens the file in the QuickLook viewer
#
# Arg 1 should be the name of the image you want to generate.
# Convention is to make it the same as the chapter number 
dotnet run > test_image/$@.ppm && test_image/qlf test_image/$@.ppm