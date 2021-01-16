
trap "exit" INT TERM ERR
trap "kill 0" EXIT

# Number of users each instance generates
NR_USERS_GEN=1
NR_CONTACTS=1
# Number of python instances
INSTANCES=100

for ((i=0; i<$INSTANCES; i++))
do
    echo "running $i..."
	python3 rest-prepared.py $i $NR_USERS_GEN "user$i-inst" $NR_CONTACTS &
done

wait
