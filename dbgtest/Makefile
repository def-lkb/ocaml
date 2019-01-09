all: dbgtest
	./dbgtest

clean:
	rm -f *.o *.cm* dbgtest

SOURCE= \
	printer.mli printer.ml \
	dbgprim_stubs.c dbgprim.mli dbgprim.ml \
 	symbol_loader.mli symbol_loader.ml dbgtest.ml

dbgtest: $(SOURCE) | ../ocamlc
	../ocamlc -custom -g -o dbgtest -I \
		../utils -I ../parsing -I ../typing -I ../bytecomp \
		../compilerlibs/ocamlcommon.cma $^

.PHONY: all
