all : cesame

cesame :parser.cmo scanner.cmo ast.cmo sast.cmo semant.cmo cesame.cmo
	ocamlc -w A -o cesame $^

%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

ast.mli : ast.ml
	ocamlc -i $^ > ast.mli
# Depedencies from ocamldep
cesame.cmo : \
    semant.cmo \
    sast.cmo \
    ast.cmo
cesame.cmx : \
    semant.cmx \
    sast.cmx \
    ast.cmx
sast.cmo : \
    ast.cmo
sast.cmx : \
    ast.cmx
semant.cmo : \
    sast.cmo \
    ast.cmo
semant.cmx : \
    sast.cmx \
    ast.cmx
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx


##############################


.PHONY : clean
clean :
	rm -rf ast.mli *.cmi *.cmo parser.ml parser.mli scanner.ml cesame