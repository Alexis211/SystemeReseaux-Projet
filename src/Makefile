CAML=example.ml primes.ml \
	 	manager.ml poolserver.ml poolclient.ml \
		kahn_pipe.ml kahn_seq.ml kahn_stdio.ml \
		proto.ml util.ml

BIN=example.native primes.native \
		manager.native poolserver.native poolclient.native

all: $(CAML)
	ocamlbuild $(BIN)


clean:
	ocamlbuild -clean

