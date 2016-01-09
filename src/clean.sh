#!/bin/bash
# Clean up the build files

rm *.o *.hi

cd Command
rm *.o *.hi
cd ..

cd Relay
rm *.o *.hi
cd ..

cd Routing
rm *.o *.hi
cd ..

cd Debug
rm *.o *.hi
cd PacketParsing
rm *.o *.hi
cd ..
cd ..
