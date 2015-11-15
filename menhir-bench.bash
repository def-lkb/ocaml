#!/bin/bash

test_parse() {
  for i in `seq 1 $1`
  do
      OCAML_PARSERS=$3 ./$2 -c -stop-after-parse ocamlbuild/*.mli ocamlbuild/*.ml
  done
}

test_type() {
  for i in `seq 1 $1`
  do
      OCAML_PARSERS=$3 ./$2 -I ocamlbuild -c -i ocamlbuild/*.mli ocamlbuild/*.ml > /dev/null
  done
}

test_compile() {
  for i in `seq 1 $1`
  do
      make -C ocamlbuild clean > /dev/null;
      OCAML_PARSERS=$3 OCAMLC=../$2 make -C ocamlbuild all > /dev/null
  done
}

# best of N
BESTOF=${BESTOF:-5}
bench() {
  for i in `seq 1 $BESTOF`
  do
      time ($1 $2 $3 $4)
  done 2>&1 | grep real | cut -f 2 | sort | head -n 1
}

# This beautiful function is an idea of Frédéric Bour
seconds_number() {
    echo $1 | sed 's/m/ * 60 +/' | sed 's/s//' | bc -l
}

ratio() {
    echo "scale=4; $(seconds_number $2) / $(seconds_number $1)" | bc
}


cat <<EOF

  This is a benchmark of Yacc- and Menhir-generated parsers used in
  the OCaml compiler. We compare performances for free usage modes:
  - parse only
  - parse and type-check the parsed code
  - full compile with the bytecode compiler

  (This script assumes that it is run from the source tree of an OCaml
   compiler that has already been installed.)

  The code used for this test are the sources of ocamlbuild. We
  compare the performances of the native-compiled and
  bytecode-compiled (bytecode) compiler, ocamlc.opt and
  ocamlc. Because the Menhir runtime is implemented in OCaml
  (while the OCamlyacc runtime is C code), we expect the Menhir
  overhead to be larger in bytecode mode.

  Each test iterates its computation enough time to run in around one
  second on my machine. Each test is run $BESTOF times, with only the
  best time of five reported. You can change the number of "best of"
  runs by setting the BESTOF environment variable.

EOF

run_test_with_compiler() {
  echo -n "yacc:      "
  YACC=$(bench $2 $3 $1 yacc)
  echo $YACC
  echo -n "menhir:    "
  MENHIR=$(bench $2 $3 $1 menhir)
  echo $MENHIR
  echo "m/y ratio: $(ratio $YACC $MENHIR)"
  echo
}

run_test() {
  echo "# $1"
  echo "## native ($3 iterations)"
  (run_test_with_compiler ocamlc.opt $2 $3)
  echo "## byte ($4 iterations)"
  (run_test_with_compiler ocamlc $2 $4)
}

run_test "Parse only" test_parse 10 3

run_test "Parse and Type" test_type 2 1

run_test "Full compile" test_compile 1 1
