.PHONY: clean all run
.DEFAULT: all

all:run 

objC.cmi: objC.mli
	ocamlopt -c objC.mli
objC.cmx, objC.o: objC.ml
	ocamlopt -c objC.ml
objC_imp.o: objC.ml
	ocamlopt -c objC_imp.c

test.opt: test.ml objC_imp.o objC.cmi objC.o
	ocamlopt -cclib "-framework Foundation" -cclib "-framework OpenGL" -cclib "-lobjc" -o test.opt test.ml objC.o objC_imp.o

run:test.opt
	otool -L test.opt
	./test.opt

clean:
	rm *.cm{i,o,x} *.o *.opt a.out

