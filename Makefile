.PHONY: clean all run
.DEFAULT: run

run: test.opt
	#otool -L test.opt
	./test.opt

all:test.opt 

objC.cmi: objC.mli
	ocamlopt -c objC.mli
objC.cmx: objC.ml
	ocamlopt -c objC.ml
objC_imp.o: objC_imp.m
	clang -c -I "/opt/local/lib/ocaml" objC_imp.m

test.opt: test.ml objC_imp.o objC.cmi objC.cmx
	ocamlopt -verbose -cclib "-framework Foundation" -o test.opt objC.cmx objC_imp.o test.ml

clean:
	rm *.cm{i,o,x} *.o *.opt a.out

