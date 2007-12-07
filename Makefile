opt: BinaryFileParse.cmxa
	ocamlopt.opt -w a -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml alphabet.ml -o run/alphabet

BinaryFileParse.cmx:
	ocamlopt.opt -w a -rectypes -c BinaryFileParse.ml -o BinaryFileParse.cmx

BinaryFileParse.cmxa: BinaryFileParse.cmx
	ocamlopt.opt -w a -rectypes -a BinaryFileParse.cmx -o BinaryFileParse.cmxa

fft.ml: BinaryFileParse.cmxa
	ocamlopt.opt -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -o fft.out

%.ml: BinaryFileParse.cmxa
	ocamlopt.opt  -w a unix.cmxa -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml $@ -o run/$*

clean:
	rm -f *.o *.cm* *~ run/* *.a

clean-results:
	rm -f resultsonth2.png results/*

clean-all: clean clean-results
	rm -f fft.out struct_sontheorique2

test:
	rm -f *.o *.cm* *~ *.a
