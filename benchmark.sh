#!/bin/bash

rm -rf *.txt results
mkdir -p results

function benchmark () {
    # Running the independent
    for k in $2 ; do
	for i in $1 ; do
	    processors="2 4 8 16"
	    for j in ${processors} ; do
		file_name=$(echo $i | awk -F '/' '{print $2}' | awk -F '.' '{print $1}')
		search=$(echo $k | awk -F '/' '{print $2}' | awk -F '.' '{print $1}')
		echo $k -timeout 30000 -processors $j $i
		sem -j4 $k -timeout 10000 -processors $j $i > ./results/$file_name.$search.$j.txt
	    done
	done
    done
}

IND=`find ../ -iname 'Independent*.gxl'`
RAND=`find ../ -iname 'Random*.gxl'`
FORK=`find ../ -iname 'Fork_Node*.gxl'`
JOIN=`find ../ -iname 'JOIN*.gxl'`
STENCIL=`find ../ -iname 'STENCIL*.gxl'`

# Do independent benchmark 
benchmark "$IND" "./binary_search ./reduce_search"

# Do fork benchmark 
benchmark  "$FORK" "./binary_search ./reduce_search"

# Do join benchmark 
benchmark "$JOIN" "./binary_search ./reduce_search"

# Do STENCIL benchmark 
benchmark "$STENCIL" "./binary_search ./reduce_search"


# # # Do random benchmark 
# benchmark "$RAND" "./binary_search ./reduce_search"

