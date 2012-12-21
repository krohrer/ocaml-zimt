.PHONY: default all top run test clean clobber

OCAMLBUILD=ocamlbuild -use-ocamlfind
PROG=zimt
TEST=test
TOP=zimt

default: test

all: top test run

top:
	$(OCAMLBUILD) $(TOP).top
	./$(TOP).top -I _build

test:
	$(OCAMLBUILD) $(TEST).byte
	./$(TEST).byte

run: $(PROG).native
	./$^

$(PROG).native: *.ml *.mli
	$(OCAMLBUILD) $@

$(PROG).byte: *.ml *.mli
	$(OCAMLBUILD) $@

clean:
	$(OCAMLBUILD) -clean

clobber: clean
	rm -f *~ *.annot *.native *.byte
