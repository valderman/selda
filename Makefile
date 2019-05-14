PACKAGES=selda selda-sqlite selda-postgresql selda-json
.PHONY: help build license deps travischeck haddock check test selda json pgtest sqlite postgres repl upload-selda upload travis-pgconnectinfo tags

help:
	@echo "Available targets:"
	@echo "build        - build packages"
	@echo "test         - build packages and run tests with SQLite"
	@echo "pgtest       - build packages and run tests with PostgreSQL"
	@echo "repl         - start ghci"
	@echo "check        - build package, run tests, do a cabal sanity check"
	@echo "travischeck  - like check, but with appropriate PGConnectInfo"
	@echo "selda        - build core Selda package"
	@echo "sqlite       - build sqlite backend"
	@echo "json         - build json extensions"
	@echo "postgres     - build postgres backend"
	@echo "upload       - upload packages to Hackage"
	@echo "upload-selda - upload only the main selda package"
	@echo "haddock      - build Haddock docs"
	@echo "tags         - build tags file for emacs"

build: selda sqlite postgres json

travis-pgconnectinfo:
	echo '{-# LANGUAGE OverloadedStrings #-}' > selda-tests/PGConnectInfo.hs
	echo 'module PGConnectInfo where' >> selda-tests/PGConnectInfo.hs
	echo 'import Database.Selda.PostgreSQL' >> selda-tests/PGConnectInfo.hs
	echo 'pgConnectInfo = ("test" `on` "localhost"){pgUsername = Just "postgres"}'  >> selda-tests/PGConnectInfo.hs

travischeck: travis-pgconnectinfo
	make check

license:
	for package in $(PACKAGES) ; do \
		cp -f ./LICENSE ./$$package/LICENSE ; \
	done

haddock:
	cabal v2-haddock $(PACKAGES)

check: test pgtest haddock
	cabal v2-run selda-changelog md
	cabal v2-clean
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  cabal check ; \
	  cd .. ; \
	done
	cabal v2-sdist $(PACKAGES)
	cabal v2-configure -f-localcache selda
	cabal v2-build selda

tags:
	hasktags --etags selda/src selda-sqlite/src selda-postgresql/src selda-json/src selda-tests/test

test: selda sqlite
	cd ./selda-tests && cabal v2-configure --enable-tests
	cd ./selda-tests && cabal v2-test

pgtest: selda postgres
	cd ./selda-tests && cabal v2-configure --enable-tests -fpostgres
	cd ./selda-tests && cabal v2-test

selda: license
	cp -f README.md ./selda/README.md
	cabal v2-build selda
	make tags ; true

json: license
	cabal v2-build selda-json
	make tags ; true

sqlite: license
	cabal v2-build selda-sqlite
	make tags ; true

postgres: license
	cabal v2-build selda-postgresql
	make tags ; true

repl:
	cabal v2-repl --ghc-options="-XOverloadedStrings" selda

upload-selda: check
	cabal v2-run selda-changelog validate
	cabal v2-run selda-changelog tag
	cabal upload ./dist-newstyle/sdist/selda-0.*.tar.gz
	git push
	git push --tags

upload: check
	cabal v2-run selda-changelog validate
	cabal v2-run selda-changelog tag
	cabal upload $$(for pkg in $(PACKAGES) ; do echo ./dist-newstyle/sdist/$$pkg-0.*.tar.gz ; done)
	git push
	git push --tags
