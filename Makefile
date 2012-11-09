.PHONY: clean all run clobber dump
.DEFAULT: run

run: test.opt
	./test.opt

dump: testopt
	otool -L test.opt

all:test.opt 

objC.cmi: objC.mli
	ocamlopt -annot -c objC.mli
objC.cmx: objC.ml
	ocamlopt -annot -c objC.ml
objC_imp.o: objC_imp.m
	clang -c -I "/opt/local/lib/ocaml" objC_imp.m

test.opt: test.ml objC_imp.o objC.cmi objC.cmx
	ocamlopt -verbose -cclib "-framework Foundation" -o test.opt objC.cmx objC_imp.o test.ml

clean:
	rm -f *.cm{i,o,x} *.o *.opt a.out

clobber: clean
	rm -f *~
