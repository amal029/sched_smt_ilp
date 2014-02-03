CC=ocamlopt -annot
SRC1=gxl2smt.ml
SRC2=smt2gxl.ml
TEST=test.ml
TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
WHERELIB=`ocamlfind query pa_where`
Z3LIB=../Z3/src/api/ml/old
CAMLIDL=../Z3/CAMLIDL/camlidl-1.05/runtime

all: gxl2smt smt2gxl test

gxl2smt:
	ocamlfind $(CC) -pp "camlp4o -I $(TYPECONV) -I $(SEXPLIB) -I	\
	$(WHERELIB) pa_type_conv.cma pa_sexp_conv.cma pa_macro.cmo	\
	pa_where.cma -UDEBUG -UTDEBUG" -o $@ -linkpkg -package		\
	batteries -package sexplib -package xml-light -package		\
	gxl-light -package pretty -I $(Z3LIB) z3.cmxa -ccopt		\
	-L$(CAMLIDL) $(SRC1)

smt2gxl:
	ocamlfind $(CC) -pp "camlp4o -I $(TYPECONV) -I $(SEXPLIB) -I	\
	$(WHERELIB) pa_type_conv.cma pa_sexp_conv.cma pa_macro.cmo	\
	pa_where.cma -UDEBUG -UTDEBUG" -o $@ -linkpkg -package		\
	batteries -package sexplib -package xml-light -package		\
	gxl-light -package pretty $(SRC2)

clean:
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS	\
	*.mli *.cm* *.o systemjc *.xml *.annot *_spi* *_ver*		\
	*.pml.trail gxl2smt *.gxl *.result smt2gxl 
