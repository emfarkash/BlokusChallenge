PKGS=utop ounit qtest yojson lwt lwt_ppx menhir ansiterminal lambda-term merlin ocp-indent user-setup bisect_ppx-ocamlbuild opium.0.18.0 ppx_yojson_conv ezjsonm odoc

default: build
	utop

build:
	cd backend && dune build bin/main.exe --profile release

test:
	cd backend && dune exec bin/test.exe

deps-ocaml:
	opam install -y $(PKGS)

deps:
	opam install -y $(PKGS) && (cd game-interface && npm install)

zip: 
	zip -r blokus.zip Makefile INSTALL.md ./backend/* ./game-interface/* -x *node_modules/* -x *_build/* 

run-backend:
	cd backend && dune exec bin/main.exe --profile release -- -p 5000

run-frontend:
	cd game-interface && npm start

start:
	(cd backend && dune exec bin/main.exe --profile release -- -p 5000) & (cd game-interface && npm start)

docs:
	cd backend && dune build @doc-private
