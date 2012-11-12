.PHONY: clean all run clobber dump
.DEFAULT: run

run: test.opt
	./test.opt

dump: testopt
	otool -L test.opt

all:test.opt 

objC.cmi: objC.mli
	ocamlopt -c objC.mli
objC.cmx objC.o: objC.ml objC.cmi
	ocamlopt -c objC.ml
objC.cmxa: objC.cmx objC.cmi objC.o objC_imp.o
	ocamlopt -a -o objC.cmxa objC.cmx objC.o objC_imp.o
objC.cmo: objC.ml objC.cmi
	ocamlc -c objC.ml
objC.cma: objC.cmo objC.cmi objC.o objC_imp.o
	ocamlc -a -custom -cclib "-framework Foundation" -o objC.cma objC.cmo objC.o objC_imp.o

objC_imp.o: objC_imp.m
	clang -c -I `ocamlc -where` objC_imp.m

test.opt: test.ml objC_imp.o objC.cmi objC.cmx
	ocamlopt -cclib "-framework Foundation" -o test.opt objC.cmx objC_imp.o test.ml

toplevel: objC.cma
	ocamlmktop -custom -cclib "-framework Foundation" -o toplevel objC.cmo objC_imp.o

clean:
	rm -f *.cm{i,o,x,a} *.o *.opt a.out

clobber: clean
	rm -f *~ *.annot
