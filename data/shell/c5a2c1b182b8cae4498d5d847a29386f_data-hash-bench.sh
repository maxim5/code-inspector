#!/bin/sh

DATABYTES=$((1024*1024*1024))

for name in goulburn hsieh jenkins phong korzendorfer1 ; do
	echo "# $name; $DATABYTES total data per size"> bench-$name.data
	for size in 1 3 4 8 9 16 31 32 64 ; do
		../benchtool $name $DATABYTES $size >> bench-$name.data
	done
done
