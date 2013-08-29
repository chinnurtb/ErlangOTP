.SUFFIXES: .erl .beam .ear

.erl.beam:
	erlc -W $<

all: code store

doc: wiki.html

code:
	cd wiki-14.0; make
	cd pico-11.0; make


wiki.html: wiki.ehtml  ${MODS:%=%.erl}
	ehtml2html wiki.ehtml

clean:
	rm -rf *.beam one two erl_crash.dump
	cd wiki-14.0; make clean
	cd pico-11.0; make clean

publish: wiki.html 
	make clean
	rm -rf store
	cp -R wiki-14.0/store store
	cd ..; tar hcvf - wiki | gzip > wiki.tgz
	mkdir -p /home/joe/public_html/tutorials/wiki
	cp ../wiki.tgz  /home/joe/public_html/tutorials	
	cp wiki.html wiki.sh /home/joe/public_html/tutorials/wiki
