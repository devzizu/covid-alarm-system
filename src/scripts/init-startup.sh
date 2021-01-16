
trap "exit" INT TERM ERR
trap "kill 0" EXIT

echo "running central server..."
cd ../backend/
make run_central &

echo "running rest api..."
cd ../rest/Diretorio
make run &

wait
