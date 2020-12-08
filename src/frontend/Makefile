.PHONY: *.erl

BEAMS := $(shell find _build -name "ebin" | tr '\n' ' ')
FRONTEND = _build/frontend/ebin

RUN_ARGS = erl -pa ${BEAMS}

# Compile all erlang modules
all: clean *.erl

*.erl:
	@echo "Compiling $@..."
	@erlc $@
	$(eval NAME := $(shell basename $@ .erl)) 
	@mv ${NAME}.beam ${FRONTEND}

run:
	@echo "Running beams ${BEAMS}..."
	${RUN_ARGS}

clean:
	@echo "Cleaning build files..."
	@rm -f ${FRONTEND}/*.beam *.dump
	@mkdir -p ${FRONTEND}
