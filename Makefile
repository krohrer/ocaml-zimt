.PHONY: clean all run clobber dump

all: c_PrettyPrinter.cmo c_Untyped.cmo
#all:test.opt objc.top foreign.top foreign.opt cLang.opt cLang.top

c_PrettyPrinter.cmo: c_PrettyPrinter.ml c_PrettyPrinter.cmi c_UntypedAST.cmi c_Untyped.cmi
	ocamlc -c c_PrettyPrinter.ml
c_PrettyPrinter.cmi: c_PrettyPrinter.mli c_UntypedAST.cmi
	ocamlc -c c_PrettyPrinter.mli

c_Untyped.cmo: c_Untyped.ml c_Untyped.cmi
	ocamlc -c c_Untyped.ml
c_Untyped.cmi: c_Untyped.mli
	ocamlc -c c_Untyped.mli

c_UntypedAST.cmi: c_UntypedAST.mli
	ocamlc -c c_UntypedAST.mli

cTyped.cmi: cTyped.mli
	ocamlc -c cTyped.mli

cLang.opt: cLang.cmx cLang.cmi
	ocamlopt -o cLang.opt cLang.cmx

cLang.top: cLang.cmo cLang.cmi
	ocamlfind ocamlmktop -syntax camlp4o -package monad -o cLang.top cLang.cmo

cLang.cmx cLang.o: cLang.ml cLang.cmi
	ocamlopt -c cLang.ml
cLang.cmi: cLang.mli
	ocamlopt -c cLang.mli
cLang.cmo: cLang.ml cLang.cmi
	ocamlc -c cLang.ml

dump: testopt
	otool -L test.opt

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
