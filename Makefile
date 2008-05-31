opt:
	(cd src; ${MAKE} $@)

%:
	(cd src; ${MAKE} $@)

clean:
	rm -f *~ run/* src/*.o src/*.cm* src/*~ src/*.a

clean-results:
	rm -f results/*

distclean: clean clean-results
	rm -f src/fft.out src/*.omc *.omc


