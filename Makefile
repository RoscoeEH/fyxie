build:
	opam exec -- dune build

run:
	opam exec -- dune exec bin/main.exe

clean:
	opam exec -- dune clean

test:
	opam exec -- dune runtest

fmt:
	opam exec -- dune fmt

install:
	opam exec -- dune install

config:
	@if opam switch show | grep -q "$(PWD)"; then \
	  echo "==> Updating opam switch..."; \
	  opam install . --deps-only --locked -y; \
	else \
	  echo "==> Creating opam switch..."; \
	  opam switch create . --deps-only --locked -y; \
	fi
