# Makefile para correr o maven, compilar, construir jar, etc...
# Para depois se quiserem testar

CENTRALSERVER_MAIN=app.central.CentralServer
DISTRICTSERVER_MAIN=app.district.DistrictServer
LAYER1_MAIN=app.broker.DynamicBrokerLayer1
LAYER2_MAIN=app.broker.DynamicBrokerLayer2
TESTE_LAYERS=app.test.GenerateLayers

# Displays compile options
default:
	@echo "Makefile options:"
	@echo "[1] compile project   : make compile"
	@echo "[2] build package jar : make package"
	@echo "[3] run with args     : make run"

# Compile changes in code and install target/ folder
compile:
	mvn compile
	mvn clean install
	
# Build maven package (jar file)
package:
	mvn package

# Run project with program arguments

run_central:
	@clear
	mvn exec:java -Dexec.mainClass=$(CENTRALSERVER_MAIN) 

run_district:
	@clear
	@read -p "Program arguments: " args; \
	mvn exec:java -Dexec.mainClass=$(DISTRICTSERVER_MAIN) -Dexec.args="$$args"

run_layer1:
	@clear
	mvn exec:java -Dexec.mainClass=$(LAYER1_MAIN) 

run_layer2:
	@clear
	mvn exec:java -Dexec.mainClass=$(LAYER2_MAIN) 

run_test_layers:
	@clear
	@read -p "Program arguments: " args; \
	mvn exec:java -Dexec.mainClass=$(TESTE_LAYERS) -Dexec.args="$$args"

