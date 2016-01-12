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

cd Control
cd IO
rm *.o *.hi
cd Builder
rm *.o *.hi
cd ..
cd ..
cd Concurrent
rm *.o *.hi
cd ..
cd ..

cd Debug
rm *.o *.hi
cd PacketParsing
rm *.o *.hi
cd ..
cd ..
