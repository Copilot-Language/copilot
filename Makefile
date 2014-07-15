all:
#cabal configure
	cabal build
	ln -f --symbolic ./dist/build/kind-tests/kind-tests ./run

clean:
	cabal clean
	rm run
