opt: BinaryParse.cmxa
	ocamlopt.opt -unsafe -ffast-math -inline 2 unix.cmxa -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml alphabet.ml -o run/alphabet

BinaryParse.cmx:
	ocamlopt.opt -rectypes -c BinaryParse.ml -o BinaryParse.cmx

BinaryParse.cmxa: BinaryParse.cmx
	ocamlopt.opt -rectypes -a BinaryParse.cmx -o BinaryParse.cmxa

fft.out: BinaryParse.cmxa
	ocamlopt.opt -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -o fft.out

%.ml: BinaryParse.cmxa
	ocamlopt.opt -unsafe -ffast-math -inline 2 unix.cmxa -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml $@ -o run/$*

clean:
	rm -f *.o *.cm* *~ run/* *.a

clean-results:
	rm -f resultsonth2.png results/*

maintainerclean: clean clean-results
	rm -f fft.out struct_sontheorique2

distclean: maintainerclean

test:
	rm -f *.o *.cm* *~ *.a
