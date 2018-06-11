all : 
	ocamlbuild picoSMT.native
.phony : clean
clean : 
	ocamlbuild -clean
