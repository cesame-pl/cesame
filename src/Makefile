.PHONY : all
all : clean cesame # You can also run "make" in the src directory.

.PHONY : cesame
cesame : 
	ocamlbuild -pkgs llvm cesame.native
	mv cesame.native cesame

.PHONY : clean
clean :
	ocamlbuild -clean