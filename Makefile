all : clean cesame  # Automatically clean first "rm -f *.cmi *.cmo"
 
cesame :parser.cmo scanner.cmo ast.cmo sast.cmo semant.cmo cesame.cmo
	ocamlc -w A -o cesame $^
	rm -f *.cmi *.cmo

%.cmo : ./src/%.ml
	ocamlc -w A -c $< -o $@

%.cmi : ./src/%.mli
	ocamlc -w A -c $< -o $@

./src/scanner.ml : ./src/scanner.mll
	ocamllex $^

./src/parser.ml ./src/parser.mli : ./src/parser.mly
	ocamlyacc $^

./src/ast.mli : ./src/ast.ml
	ocamlc -i $^ > ./src/ast.mli

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
	rm -rf ./src/*.mli ./src/*.cmi ./src/*.cmo *.cmi *.cmo \
    ./src/parser.ml ./src/parser.mli ./src/scanner.ml cesame