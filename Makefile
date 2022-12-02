all:
	cd src && $(MAKE)

clean:
	rm latc_llvm && rm latc && cd src && $(MAKE) clean

distclean: clean
	-rm latc_llvm latc
