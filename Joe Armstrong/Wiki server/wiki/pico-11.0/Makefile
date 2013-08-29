.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

MODS = pico_utils  pico_socket_server pico_http_server pico_test

all: pico 

pico: ${MODS:%=%.beam}

clean:
	rm -f *~ *.beam erl_crash.dump


dist:
	mkdir pico-11.0
	cp Makefile *.erl start_4999 README  pico-11.0
	gtar -zcvf pico-11.0.tgz pico-11.0
	rm -rf pico-11.0
