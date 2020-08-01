Selda
=====

[![Join the chat at https://gitter.im/selda-hs/Lobby](https://badges.gitter.im/selda-hs/Lobby.svg)](https://gitter.im/selda-hs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Hackage](https://img.shields.io/hackage/v/selda.svg?style=flat)](http://hackage.haskell.org/package/selda)
[![IRC channel](https://img.shields.io/badge/IRC-%23selda-1e72ff.svg?style=flat)](https://www.irccloud.com/invite?channel=%23selda&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1)
![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)
![Haskell CI](https://github.com/valderman/selda/workflows/Haskell%20CI/badge.svg)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/selda.svg)](https://packdeps.haskellers.com/feed?needle=selda)


What is Selda?
==============
[Selda](https://selda.link) is a Haskell library for interacting with SQL-based relational databases.
It was inspired by [LINQ](https://en.wikipedia.org/wiki/Language_Integrated_Query) and
[Opaleye](http://hackage.haskell.org/package/opaleye).


Features
========

* Monadic interface.
* Portable: backends for SQLite and PostgreSQL.
* Generic: easy integration with your existing Haskell types.
* Creating, dropping and querying tables using type-safe database schemas.
* Typed query language with products, filtering, joins and aggregation.
* Inserting, updating and deleting rows from tables.
* Conditional insert/update.
* Transactions, uniqueness constraints and foreign keys.
* Type-safe, backend-specific functionality, such as JSON lookups.
* Seamless prepared statements.
* Lightweight and modular: few dependencies, and non-essential features are
  optional or split into add-on packages.


Getting started
===============

Install the `selda` package from Hackage, as well as at least one of the
backends:

    $ cabal update
    $ cabal install selda selda-sqlite selda-postgresql

Then, read [the tutorial](https://selda.link/tutorial).
The [API documentation](http://hackage.haskell.org/package/selda) will probably
also come in handy.


Requirements
============

Selda requires GHC 8.0+, as well as SQLite 3.7.11+ or PostgreSQL 9.4+.
To build the SQLite backend, you need a C compiler installed.
To build the PostgreSQL backend, you need the `libpq` development libraries
installed (`libpq-dev` on Debian-based Linux distributions).

Hacking
=======

Contributing
------------

All forms of contributions are welcome!

If you have a bug to report, please try to include as much information as
possible, preferably including:

* A brief description (one or two sentences) of the bug.
* The version of Selda+backend where the bug was found.
* A step-by-step guide to reproduce the bug.
* The *expected* result from following these steps.
* What *actually* happens when following the steps.
* Which component contains the bug (selda, selda-sqlite or selda-postgresql),
  if you're reasonably sure about where the bug is.

Bonus points for a small code example that illustrates the problem.

If you want to contribute code, please consult the following checklist before
sending a pull request:

* Does the code build with a recent version of GHC?
* Do all the tests pass?
* Have you added any tests covering your code?

If you want to contribute code but don't really know where to begin,
issues tagged [good first issue](https://github.com/valderman/selda/issues?q=is%3Aissue+is%3Aopen+label%3A%22good+first+issue%22) are a good start.


Setting up the build environment
--------------------------------

From the repository root:

* Install `libpq-dev` from your package manager.
    This is required to build the PostgreSQL backend.
* Make sure you're running a cabal version that supports v2-style commands.
* Familiarise yourself with the various targets in the makefile.
    The dependencies between Selda, the backends and the tests are slightly
    complex, so straight-up cabal is too quirky for day to day hacking.


PostgreSQL backend testing with Docker
--------------------------------------

To test the PostgreSQL backend, use the provided `pgtest-compose.yml` docker-compose file:
```
sudo docker-compose -f pgtest-compose.yml up -d
make pgtest
sudo docker-compose -f pgtest-compose.yml down
```


TODOs
-----

Features that would be nice to have but are not yet implemented.

* Monadic if/else
* Streaming
* MySQL/MariaDB backend
* MSSQL backend
