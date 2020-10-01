.PHONY: all run clean

all: vfdl.cabal
	cabal new-build

run: vfdl.cabal
	cabal new-run fdlc

clean: vfdl.cabal
	cabal new-clean

vfdl.cabal: package.yaml
	hpack
