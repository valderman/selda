PACKAGES=selda selda-sqlite selda-postgresql selda-json
.PHONY: help build license haddock check test selda json pgtest pgtest-docker sqlite postgres repl upload-selda upload
CABAL_BUILDFLAGS ?=
CABAL ?= cabal

help:
	@echo "Available targets:"
	@echo "build         - build packages"
	@echo "test          - build packages and run tests with SQLite"
	@echo "pgtest        - build packages and run tests with PostgreSQL"
	@echo "pgtest-docker - build packages and run tests with a Docker PostgreSQL image"
	@echo "repl          - start ghci"
	@echo "check         - build package, run tests, do a cabal sanity check"
	@echo "selda         - build core Selda package"
	@echo "sqlite        - build sqlite backend"
	@echo "json          - build json extensions"
	@echo "postgres      - build postgres backend"
	@echo "upload        - upload packages to Hackage"
	@echo "upload-selda  - upload only the main selda package"
	@echo "haddock       - build Haddock docs"

build: selda sqlite postgres json

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

test: selda sqlite
	cd ./selda-tests && ${CABAL} v2-configure --enable-tests $(CABAL_BUILDFLAGS)
	cd ./selda-tests && ${CABAL} v2-test $(CABAL_BUILDFLAGS)

pgtest: selda postgres
	cd ./selda-tests && ${CABAL} v2-configure --enable-tests -fpostgres $(CABAL_BUILDFLAGS)
	cd ./selda-tests && ${CABAL} v2-test $(CABAL_BUILDFLAGS)

pgtest-docker: selda postgres .has_postgres_docker_image
	docker run --privileged --rm --name selda-postgres -p 5432:5432 -e POSTGRES_PASSWORD=password -d docker.io/postgres:16
	cd ./selda-tests && ${CABAL} v2-configure --enable-tests -fpostgres $(CABAL_BUILDFLAGS)
	cd ./selda-tests && ${CABAL} v2-test $(CABAL_BUILDFLAGS)
	docker stop selda-postgres

.has_postgres_docker_image:
	docker pull docker.io/postgres
	touch .has_postgres_docker_image

selda: license
	cp -f README.md ./selda/README.md
	${CABAL} v2-build selda $(CABAL_BUILDFLAGS)

json: license
	${CABAL} v2-build selda-json $(CABAL_BUILDFLAGS)

sqlite: license
	${CABAL} v2-build selda-sqlite $(CABAL_BUILDFLAGS)

postgres: license
	${CABAL} v2-build selda-postgresql $(CABAL_BUILDFLAGS)

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
