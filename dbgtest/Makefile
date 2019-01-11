all: run-eval

run-eval: eval
	./eval

run-dbgtest: dbgtest
	./dbgtest

clean:
	rm -f *.o *.cm* dbgtest

SOURCE= \
	printer.mli printer.ml \
	dbgprim_stubs.c dbgprim.mli dbgprim.ml \
 	symbol_loader.mli symbol_loader.ml

dbgtest: ../compilerlibs/ocamlcommon.cma ../compilerlibs/ocamltoplevel.cma $(SOURCE) dbgtest.ml
	../ocamlc -custom -g -o $@ -I \
		../utils -I ../parsing -I ../typing -I ../bytecomp -I ../toplevel \
		$^

eval: ../compilerlibs/ocamlcommon.cma ../compilerlibs/ocamlbytecomp.cma ../compilerlibs/ocamltoplevel.cma $(SOURCE) eval.ml
	../ocamlc -custom -g -o $@ \
		-I ../driver \
		-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../toplevel \
		$^

.PHONY: all run-eval run-dbgtest
