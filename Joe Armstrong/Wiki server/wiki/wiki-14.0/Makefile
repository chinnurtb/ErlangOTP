.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

MODS = wiki find diff wiki_templates wiki_to_html wiki_format_txt wiki_split\
       wiki_utils

all: code 

code: ${MODS:%=%.beam}

clean:
	rm -f *~ *.beam erl_crash.dump

dist:
	mkdir wiki-14.0
	cp Makefile *.erl README wiki.sh wiki-14.0
	cp -r store wiki-14.0
	gtar -zcvf wiki-14.0.tgz wiki-14.0
	rm -rf wiki-14.0


