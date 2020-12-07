.PHONY: *.erl
RUN_ARGS = erl -pa _build/default/lib/chumak/ebin

# Compile all erlang modules

all: clean *.erl

*.erl:
	erlc $@

run: all
	${RUN_ARGS}

clean:
	rm -f *.beam *.dump