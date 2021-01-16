#!/bin/bash

trap "exit" INT TERM ERR
trap "kill 0" EXIT

cd ../backend/

# Run district servers
districts=("Braga" "Vianadocastelo" "Vilareal" "Castelobranco" "Leiria" "Lisboa" "Porto" "Evora" "Beja" "Faro" "Portalegre" "Coimbra" "Braganca" "Aveiro" "Santarem" "Setubal" "Guarda" "Viseu")

echo "running district servers..."
for ((i=0; i<${#districts[@]}; i++))
do
	d="${districts[$i]}"
	echo -e "\t$d starting..."	
	$(sleep 1 | echo $d | make run_district) &
done

wait
