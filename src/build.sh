#!/bin/bash

libs=($(ls -d */ )) 

for lib in ${libs[@]}
do
	cd $lib
	make clean
	make
	cd ..

done
