all:
	+$(MAKE) -C src
	mv src/latc_llvm ./

clean:
	+$(MAKE) distclean -C src
	rm -f latc_llvm
