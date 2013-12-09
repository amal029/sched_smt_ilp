CC=ocamlopt -annot
SRC=parse.ml
TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`

parse:
	ocamlfind $(CC) -pp "camlp4o -I $(TYPECONV) -I $(SEXPLIB)	\
	pa_type_conv.cma pa_sexp_conv.cma pa_macro.cmo -UDEBUG		\
	-DTDEBUG" -o $@ -linkpkg -package batteries -package sexplib	\
	-package xml-light -package ocaml-gxl-light -package pretty	\
	$(SRC)

clean:
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS	\
	*.mli *.cm* *.o systemjc *.xml *.annot *_spi* *_ver*		\
	*.pml.trail parse
