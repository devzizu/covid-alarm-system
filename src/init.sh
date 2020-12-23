#!/bin/bash

#DistrictsList=("Braga Viana\ Do\ Castelo Vila\ Real Castelo\ Branco Leiria Lisboa Porto Evora Beja Faro Portalegre Coimbra Braganca Aveiro Santarem Setubal Guarda Viseu)
DistrictsList=("Braga" "Porto" "Lisboa" "Viana Do Castelo")
echo "${#distro[@]}"
# Start all district servers
for dIndex in ${seq 1 ${#DistrictsList[@]}}; 
do
    echo ${DistrictsList[3]}
done