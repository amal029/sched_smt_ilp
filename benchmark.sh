#!/bin/bash

rm -rf *.txt results
mkdir -p results

count=1
function benchmark () {
    # Running the independent
    for k in $2 ; do
	for i in $1 ; do
	    processors="2 4 8 16"
	    for j in ${processors} ; do
		file_name=$(echo $i | awk -F '/' '{print $2}' | awk -F '.' '{print $1"."$2}')
		search=$(echo $k | awk -F '/' '{print $2}' | awk -F '.' '{print $1}')
		echo $k -timeout 10000 -processors $j $i
		count=$(($count + 1))
		sem -j4 $k -timeout 10000 -processors $j $i > ./results/$file_name.$search.$j"_processors".$count.txt
	    done
	done
    done
}

SERIES=`find ../ -iname 'Series*.gxl'`
PIPE=`find ../ -iname 'Pipeline*.gxl'`
IND=`find ../ -iname 'Independent*.gxl'`
RAND=`find ../ -iname 'Random*.gxl'`
FORK=`find ../ -iname 'Fork_Node*.gxl'`
JOIN=`find ../ -iname 'JOIN*.gxl'`
STENCIL=`find ../ -iname 'STENCIL*.gxl'`
INTREE=`find ../ -iname 'InTree*.gxl'`
FORKJOIN=$(find ../ -iname 'Fork_Join*.gxl')
OUTTREE=$(find ../ -iname 'OutTree*.gxl')

# # # Do random benchmark 
benchmark "$SERIES" "./binary_search"

# # # Do random benchmark 
benchmark "$PIPE" "./binary_search"

# Do independent benchmark 
# benchmark "$IND" "./binary_search ./reduce_search"
benchmark "$IND" "./binary_search"

# Do fork benchmark 
benchmark  "$FORK" "./binary_search"
# benchmark  "$FORK" "./binary_search ./reduce_search"

# Do join benchmark 
benchmark "$JOIN" "./binary_search"
# benchmark "$JOIN" "./binary_search ./reduce_search"

# Do STENCIL benchmark 
benchmark "$STENCIL" "./binary_search" 
# benchmark "$STENCIL" "./binary_search ./reduce_search"

# # # Do random benchmark 
benchmark "$RAND" "./binary_search"
# benchmark "$RAND" "./binary_search ./reduce_search"


benchmark "$INTREE" "./binary_search"
benchmark "$OUTTREE" "./binary_search"
benchmark "$FORKJOIN" "./binary_search"
