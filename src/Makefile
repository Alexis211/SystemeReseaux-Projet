OCAMLBUILD=ocamlbuild -classic-display \
		-tags annot,debug,thread \
		-libs unix

all: primes.native example.native manager.native

%.native: %.ml kahn_pipe.ml kahn_seq.ml kahn_stdio.ml proto.ml util.ml
	$(OCAMLBUILD) $@


clean:
	$(OCAMLBUILD) -clean

