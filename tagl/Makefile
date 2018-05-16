CC ?= gcc
OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt

all:
	$(OCAMLC) -c tagl_stubs.c tagl.ml
	$(OCAMLOPT) -c tagl.ml
	$(CC) -shared -o dlltagl.so tagl_stubs.o
	$(OCAMLC) -a -o tagl.cma tagl.cmo tagl_stubs.o -dllib -ltagl
	$(OCAMLOPT) -a -o tagl.cmxa tagl.cmx tagl_stubs.o

clean:
	rm -f *.a *.o *.cma *.cmxa *.cmo *.cmi *.cmx *.so
