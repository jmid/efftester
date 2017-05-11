eff:
	ocamlbuild -package qcheck src/efftester.cma
	ocamlbuild -package qcheck src/effmain.byte
	ocamlbuild -package qcheck src/effmain.native

stat:
	ocamlbuild -package qcheck src/effstat.native

clean:
	ocamlbuild -clean
	rm -f *~
	rm -f testdir/test.{ml,o,cmi,cmo,cmx} testdir/{byte,native,byte.out,native.out}
