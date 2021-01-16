#!/bin/bash

LAYERS1=$1
LAYERS2=$2

if [ $# -eq 0 ]
	then	
		LAYERS1=2
		LAYERS2=2
fi

trap "exit" INT TERM ERR
trap "kill 0" EXIT

echo "starting broker layers (l1:$LAYERS1,l2:$LAYERS2)"
cd ../backend/
$(sleep 1 | echo "$LAYERS1 $LAYERS2" | make run_test_layers)

wait
