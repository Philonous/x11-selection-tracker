.PHONY: build
build:
	stack build --install-ghc --test --no-run-tests

.PHONY: test
test:
	stack test --install-ghc

.PHONY: install
install: build
	stack install

.PHONY: clean
clean:
	stack clean

.PHONY: distclean
distclean: clean
	rm -r .stack-work
	rm x11-clipboard.cabal
