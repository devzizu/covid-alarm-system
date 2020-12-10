# Makefile para correr o maven, compilar, construir jar, etc...
# Para depois se quiserem testar

CENTRALSERVER_MAIN=app.central.CentralServer
DISTRICTSERVER_MAIN=app.district.DistrictServer

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
	#@read -p "Program arguments: " args; \
	#mvn exec:java -Dexec.mainClass=$(MAIN_CLASS) -Dexec.args="$$args"
	mvn exec:java -Dexec.mainClass=$(CENTRALSERVER_MAIN) 

run_district:
	@clear
	@read -p "Program arguments: " args; \
	mvn exec:java -Dexec.mainClass=$(DISTRICTSERVER_MAIN) -Dexec.args="$$args"