all: dbgtest
	./dbgtest

clean:
	rm -f *.o *.cm* dbgtest

SOURCE= \
	printer.mli printer.ml \
	dbgprim_stubs.c dbgprim.mli dbgprim.ml \
 	symbol_loader.mli symbol_loader.ml dbgtest.ml

dbgtest: $(SOURCE)
	ocamlfind c -custom -g -o dbgtest -package compiler-libs.common -linkpkg $^

.PHONY: all
