.PHONY: clean all run clobber dump

cLang.top: cLang.cmo cLang.cmi
	ocamlmktop -o cLang.top cLang.cmo

cLang.opt: cLang.ml cLang.cmi
	ocamlopt -o cLang.opt cLang.ml

cLang.cmx cLang.o: cLang.ml cLang.cmi
	ocamlopt -c cLang.ml
cLang.cmi: cLang.mli
	ocamlopt -c cLang.mli
cLang.cmo: cLang.ml cLang.cmi
	ocamlc -c cLang.ml

run: foreign.opt
	./foreign.opt

dump: testopt
	otool -L test.opt

all:test.opt objc.top foreign.top foreign.opt clang.opt

foreign.cmi: foreign.mli
	ocamlopt -c foreign.mli
foreign.cmx foreign.o: foreign.ml foreign.cmi
	ocamlopt -c foreign.ml
foreign.cmo: foreign.ml foreign.cmi
	ocamlc -c foreign.ml

foreign.opt: foreign.ml foreign.cmi
	ocamlopt -o foreign.opt foreign.ml

foreign.top: foreign.cmo foreign.cmi
	ocamlmktop -o foreign.top foreign.cmo


objC.cmi: objC.mli
	ocamlopt -c objC.mli
objC.cmx objC.o: objC.ml objC.cmi
	ocamlopt -c objC.ml
objC.cmo: objC.ml objC.cmi
	ocamlc -c objC.ml

objC.cmxa: objC.cmx objC.cmi objC.o objC_imp.o
	ocamlopt -a -o objC.cmxa objC.cmx objC.o objC_imp.o

objC.cma: objC.cmo objC.cmi objC.o objC_imp.o
	ocamlc -a -custom -cclib "-framework Foundation" -o objC.cma objC.cmo objC.o objC_imp.o

objC_imp.o: objC_imp.m
	clang -c -I `ocamlc -where` objC_imp.m

test.opt: test.ml objC_imp.o objC.cmi objC.cmx
	ocamlopt -cclib "-framework Foundation" -o test.opt objC.cmx objC_imp.o test.ml

objc.top: objC.cma
	ocamlmktop -custom -cclib "-framework Foundation" -o objc.top objC.cmo objC_imp.o


clean:
	rm -f *.cm{i,o,x,a,xa} *.a *.o *.opt a.out *.top

clobber: clean
	rm -f *~ *.annot
