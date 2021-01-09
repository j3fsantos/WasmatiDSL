OCAMLBUILDFLAGS=-use-ocamlfind

default:
	ocamlbuild ${OCAMLBUILDFLAGS} wasmati.otarget

clean:
	ocamlbuild ${OCAMLBUILDFLAGS} -clean