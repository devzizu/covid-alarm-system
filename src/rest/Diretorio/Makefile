# Makefile para correr o maven, compilar, construir jar, etc...
# Para depois se quiserem testar

DIRETORIO_MAIN=diretorio.DiretorioApplication
ARGS=server config.yml

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

run:
	@clear
	mvn exec:java -Dexec.mainClass=$(DIRETORIO_MAIN) -Dexec.args="${ARGS}"