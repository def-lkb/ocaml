CC ?= gcc
OCAMLC ?= ../ocamlc
OCAMLMKLIB ?= ../tools/ocamlmklib
OCAMLOPT ?= ../ocamlopt

bytecode:
	$(OCAMLC) -c tagl_stubs.c tagl.ml
	$(OCAMLMKLIB) -o tagl tagl_stubs.o
	$(OCAMLC) -a -o tagl.cma tagl.cmo tagl_stubs.o -dllib -ltagl

native:
	$(OCAMLOPT) -c tagl_stubs.c tagl.ml
	$(OCAMLOPT) -a -o tagl.cmxa tagl.cmx tagl_stubs.o

clean:
	rm -f *.a *.o *.cma *.cmxa *.cmo *.cmi *.cmx *.so

ocaml:
	../ocaml tagl.cma
