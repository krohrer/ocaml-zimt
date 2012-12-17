.PHONY: all run test clean clobber

OCAMLBUILD=ocamlbuild -use-ocamlfind
PROG=zimt
TEST=test

all: run

test: *.ml *.mli
	$(OCAMLBUILD) $(TEST).byte
	./$(TEST).byte

run: $(PROG).native
	./$(PROG).native

$(PROG).native: *.ml *.mli
	$(OCAMLBUILD) $@

$(PROG).byte: *.ml *.mli
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean

clobber: clean
	rm -f *~ *.annot *.native *.byte
