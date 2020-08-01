PACKAGES=selda selda-sqlite selda-postgresql selda-json
.PHONY: help build license deps travischeck haddock check test selda json pgtest sqlite postgres repl upload-selda upload travis-pgconnectinfo tags
CABAL_BUILDFLAGS ?=
CABAL ?= cabal

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
	echo 'pgConnectInfo = "test" `on` "localhost" `auth` ("postgres", "password")'  >> selda-tests/PGConnectInfo.hs

travischeck: travis-pgconnectinfo
	make check

license:
	for package in $(PACKAGES) ; do \
		cp -f ./LICENSE ./$$package/LICENSE ; \
	done

haddock:
	${CABAL} v2-haddock $(PACKAGES)

check: test pgtest haddock
	${CABAL} v2-run selda-changelog md
	${CABAL} v2-clean
	for pkg in $(PACKAGES) ; do \
	  cd $$pkg ; \
	  ${CABAL} check ; \
	  cd .. ; \
	done
	${CABAL} v2-sdist $(PACKAGES)
	${CABAL} v2-configure -f-localcache selda
	${CABAL} v2-build selda

tags:
	hasktags --etags selda/src selda-sqlite/src selda-postgresql/src selda-json/src selda-tests/test

test: selda sqlite
	cd ./selda-tests && ${CABAL} v2-configure --enable-tests $(CABAL_BUILDFLAGS)
	cd ./selda-tests && ${CABAL} v2-test $(CABAL_BUILDFLAGS)

pgtest: selda postgres
	cd ./selda-tests && ${CABAL} v2-configure --enable-tests -fpostgres $(CABAL_BUILDFLAGS)
	cd ./selda-tests && ${CABAL} v2-test $(CABAL_BUILDFLAGS)

selda: license
	cp -f README.md ./selda/README.md
	${CABAL} v2-build selda $(CABAL_BUILDFLAGS)
	make tags ; true

json: license
	${CABAL} v2-build selda-json $(CABAL_BUILDFLAGS)
	make tags ; true

sqlite: license
	${CABAL} v2-build selda-sqlite $(CABAL_BUILDFLAGS)
	make tags ; true

postgres: license
	${CABAL} v2-build selda-postgresql $(CABAL_BUILDFLAGS)
	make tags ; true

repl:
	${CABAL} v2-repl --ghc-options="-XOverloadedStrings" selda-sqlite $(CABAL_BUILDFLAGS)

upload-selda: check
	${CABAL} v2-run selda-changelog validate
	${CABAL} v2-run selda-changelog tag
	${CABAL} upload ./dist-newstyle/sdist/selda-0.*.tar.gz
	git push
	git push --tags

upload: check
	${CABAL} v2-run selda-changelog validate
	${CABAL} v2-run selda-changelog tag
	${CABAL} upload $$(for pkg in $(PACKAGES) ; do echo ./dist-newstyle/sdist/$$pkg-0.*.tar.gz ; done)
	git push
	git push --tags
	echo "All done!"
	echo "Don't forget to publish the package RCs and draft a GitHub release."
