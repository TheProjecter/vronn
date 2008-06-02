opt:
	(cd src; ${MAKE} $@)

wav:
	./midge/gen.sh 1 && ./midge/gen.sh 2

%:
	(cd src; ${MAKE} $@)

clean:
	rm -f *~ run/* src/*.o src/*.cm* src/*~ src/*.a

clean-results:
	rm -f results/*

distclean: clean clean-results
	rm -f src/fft.out src/*.omc *.omc


