opt: BinaryFileParse.cmxa
	ocamlopt.opt -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml dodolala.ml -o run/dodolala

BinaryFileParse.cmx:
	ocamlopt.opt -rectypes -c BinaryFileParse.ml -o BinaryFileParse.cmx

BinaryFileParse.cmxa: BinaryFileParse.cmx
	ocamlopt.opt -rectypes -a BinaryFileParse.cmx -o BinaryFileParse.cmxa

fft.ml: BinaryFileParse.cmxa
	ocamlopt.opt -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -o fft.out

%.ml: BinaryFileParse.cmxa
	ocamlopt.opt -rectypes BinaryFileParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml ajourbiais.ml $@ -o run/$*

clean:
	rm -f *.o *.cm* *~ *.a 
