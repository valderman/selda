PACKAGES=./selda ./selda-sqlite ./selda-postgresql

help:
	@echo "Available targets:"
	@echo "build    - build and install packages into sandbox"
	@echo "repl     - start ghci in sandbox"
	@echo "sqlite   - build and install sqlite backend into sandbox"
	@echo "postgres - build and install sqlite backend into sandbox"
	@echo "upload   - upload packages to Hackage"

build: cabal.sandbox.config
	cabal install $(PACKAGES)

sqlite: cabal.sandbox.config
	cabal install ./selda-sqlite

postgres: cabal.sandbox.config
	cabal install ./selda-postgresql

repl: cabal.sandbox.config
	cabal repl --ghc-options="-XOverloadedStrings"

upload: build
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  cabal sdist ; \
	  cd .. ; \
	done
	cabal upload $$(for pkg in $(PACKAGES) ; do echo $$pkg/dist/$$pkg-*.tar.gz ; done)

cabal.sandbox.config:
	cabal sandbox init
