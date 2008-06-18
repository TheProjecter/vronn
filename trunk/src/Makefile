opt: BinaryParse.cmxa fft.cmi
  ocamlopt.opt -unsafe -ffast-math unix.cmxa -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml reseaux.ml params.ml notes.ml -o ../run/notes

%.cmi:
  ocamlopt.opt -rectypes -c $*.mli -o $@

BinaryParse.cmx: BinaryParse.cmi
  ocamlopt.opt -rectypes -c BinaryParse.ml -o BinaryParse.cmx

BinaryParse.cmxa: BinaryParse.cmx
  ocamlopt.opt -rectypes -a BinaryParse.cmx -o BinaryParse.cmxa

fft.out: BinaryParse.cmxa
  ocamlopt.opt -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -o fft.out

%.ml: BinaryParse.cmxa fft.cmi
  ocamlopt.opt -unsafe -ffast-math unix.cmxa -rectypes BinaryParse.cmxa bigarray.cmxa -I +site-lib/fftw2 fftw2.cmxa -cclib -lfftw -cclib -lrfftw -cclib -lm fft.ml -I +camlimages ci_core.cmxa graphics.cmxa ci_graphics.cmxa ci_png.cmxa affichage.ml reseaux.ml params.ml $@ -o ../run/$*

