all: 
	ocamlbuild -yaccflag -v -lib unix src/main.native
	ln -f -s main.native fouine
	chmod +x fouine

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
	rm -f fouine
