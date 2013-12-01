CC=ocamlopt -annot
SRC=parse.ml

parse:
	ocamlfind $(CC) -o $@ -linkpkg -package batteries -package	\
	sexplib -package xml-light -package ocaml-gxl-light $(SRC)

clean:
	rm -rf *.ll *.lle *.bc *.s *.dot *.grf *.part* gmon.out TAGS	\
	*.mli *.cm* *.o systemjc *.xml *.annot *_spi* *_ver*		\
	*.pml.trail parse
