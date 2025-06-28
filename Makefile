build:
	dune build

run:
	dune exec bin/main.exe

clean:
	dune clean

test:
	dune runtest

fmt:
	dune fmt

install:
	dune install
