.PHONY: all top run test clean clobber

OCAMLBUILD=ocamlbuild -use-ocamlfind
PROG=zimt
TEST=test
TOP=zimt

all: run

top: *.ml *.mli
	$(OCAMLBUILD) $(TOP).top
	./$(TOP).top -I _build

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
