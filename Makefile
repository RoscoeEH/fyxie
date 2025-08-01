build:
	opam exec -- dune build

run:
	opam exec -- dune exec bin/main.exe

debug:
	opam exec -- ocamldebug _build/default/bin/main.bc

clean:
	opam exec -- dune clean

test:
	opam exec -- dune runtest

fmt:
	opam exec -- dune fmt

install:
	opam exec -- dune install

config:
	@if [ -d "_opam" ]; then \
	  echo "==> Updating opam switch..."; \
	  eval $$(opam env); \
	  opam install . --deps-only --locked -y; \
	else \
	  echo "==> Creating opam switch..."; \
	  opam switch create . ocaml-base-compiler.5.3.0; \
	  eval $$(opam env); \
	  opam install . --deps-only --locked -y; \
	fi
