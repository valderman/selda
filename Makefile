PACKAGES=./selda ./selda-sqlite ./selda-postgresql

help:
	@echo "Available targets:"
	@echo "build    - build and install packages into sandbox"
	@echo "test     - build packages and run tests with SQLite"
	@echo "pgtest   - build packages and run tests with PostgreSQL"
	@echo "repl     - start ghci in sandbox"
	@echo "check    - build package, run tests, do a cabal sanity check"
	@echo "sqlite   - build and install sqlite backend into sandbox"
	@echo "postgres - build and install sqlite backend into sandbox"
	@echo "upload   - upload packages to Hackage"

build: cabal.sandbox.config
	cabal install $(PACKAGES)

check: test pgtest
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  cabal check ; \
	  cd .. ; \
	done

test: build
	cabal install --only-dependencies --enable-tests ./selda-tests
	cd ./selda-tests && cabal configure --enable-tests
	cd ./selda-tests && cabal test

pgtest: build
	cabal install --only-dependencies --enable-tests ./selda-tests
	cd ./selda-tests && cabal configure --enable-tests -fpostgres
	cd ./selda-tests && cabal test

sqlite: cabal.sandbox.config
	cabal install ./selda-sqlite

postgres: cabal.sandbox.config
	cabal install ./selda-postgresql

repl: cabal.sandbox.config
	cabal repl --ghc-options="-XOverloadedStrings"

upload: check
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  cabal sdist ; \
	  cd .. ; \
	done
	cabal upload $$(for pkg in $(PACKAGES) ; do echo $$pkg/dist/$$pkg-*.tar.gz ; done)

cabal.sandbox.config:
	mkdir -p .cabal-sandbox
	cd .cabal-sandbox ; cabal sandbox init --sandbox .
	cd selda-tests ; cabal sandbox init --sandbox ../.cabal-sandbox
	cabal sandbox init --sandbox .cabal-sandbox
