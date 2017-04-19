PACKAGES=./selda ./selda-sqlite ./selda-postgresql

help:
	@echo "Available targets:"
	@echo "build       - build and install packages"
	@echo "test        - build packages and run tests with SQLite"
	@echo "pgtest      - build packages and run tests with PostgreSQL"
	@echo "repl        - start ghci"
	@echo "check       - build package, run tests, do a cabal sanity check"
	@echo "travischeck - like check, but with appropriate PGConnectInfo"
	@echo "sqlite      - build and install sqlite backend"
	@echo "postgres    - build and install sqlite backend"
	@echo "upload      - upload packages to Hackage"
	@echo "haddock     - build Haddock docs"
	@echo "sandbox     - create shared sandbox"

build:
	cd ./selda ; cabal configure
	cp -f README.md ./selda/README.md
	cabal install $(PACKAGES)

travischeck:
	echo '{-# LANGUAGE OverloadedStrings #-}' > selda-tests/PGConnectInfo.hs
	echo 'module PGConnectInfo where' >> selda-tests/PGConnectInfo.hs
	echo 'import Database.Selda.PostgreSQL' >> selda-tests/PGConnectInfo.hs
	echo 'pgConnectInfo = ("test" `on` "localhost"){pgUsername = Just "postgres"}'  >> selda-tests/PGConnectInfo.hs
	make check

haddock:
	cd selda ; cabal configure
	cd selda ; cabal haddock

check: test pgtest haddock
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  cabal check ; \
	  cabal sdist ; \
	  cd .. ; \
	done
	cd ./selda ; cabal configure -f-localcache
	cd ./selda ; cabal build

test: build
	cabal install --only-dependencies --enable-tests --allow-newer=time ./selda-tests
	cd ./selda-tests && cabal configure --enable-tests
	cd ./selda-tests && cabal test

pgtest: build
	cabal install --only-dependencies --enable-tests ./selda-tests
	cd ./selda-tests && cabal configure --enable-tests -fpostgres
	cd ./selda-tests && cabal test

sqlite:
	cabal install ./selda-sqlite

postgres:
	cabal install ./selda-postgresql

repl:
	cabal repl --ghc-options="-XOverloadedStrings"

upload: check
	cabal upload $$(for pkg in $(PACKAGES) ; do echo $$pkg/dist/$$pkg-*.tar.gz ; done)

sandbox: cabal.sandbox.config

cabal.sandbox.config:
	mkdir -p .cabal-sandbox
	cd .cabal-sandbox ; cabal sandbox init --sandbox .
	cd selda-tests ; cabal sandbox init --sandbox ../.cabal-sandbox
	cabal sandbox init --sandbox .cabal-sandbox
