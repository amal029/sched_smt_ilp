CC=ocamlopt -annot
SRC=gxl2smt.ml
TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
WHERELIB=`ocamlfind query pa_where`

gxl2smt:
	ocamlfind $(CC) -pp "camlp4o -I $(TYPECONV) -I $(SEXPLIB) -I	\
	$(WHERELIB) pa_type_conv.cma pa_sexp_conv.cma pa_macro.cmo	\
	pa_where.cma -UDEBUG -UTDEBUG" -o $@ -linkpkg -package		\
	batteries -package sexplib -package xml-light -package		\
	gxl-light -package pretty $(SRC)

clean:
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS	\
	*.mli *.cm* *.o systemjc *.xml *.annot *_spi* *_ver*		\
	*.pml.trail gxl2smt
