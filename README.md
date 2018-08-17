Selda
=====

[![Join the chat at https://gitter.im/selda-hs/Lobby](https://badges.gitter.im/selda-hs/Lobby.svg)](https://gitter.im/selda-hs/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Hackage](https://img.shields.io/hackage/v/selda.svg?style=flat)](http://hackage.haskell.org/package/selda)
[![IRC channel](https://img.shields.io/badge/IRC-%23selda-1e72ff.svg?style=flat)](https://www.irccloud.com/invite?channel=%23selda&amp;hostname=irc.freenode.net&amp;port=6697&amp;ssl=1)
![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)
[![Build Status](https://travis-ci.org/valderman/selda.svg?branch=master)](https://travis-ci.org/valderman/selda)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/selda.svg)


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
* Seamless prepared statements.
* Configurable, automatic, consistent in-process caching of query results.
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

Selda requires GHC 7.10+, as well as SQLite 3.7.11+ or PostgreSQL 9.6+.
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


Setting up the build environment
--------------------------------

From the repository root:

* Install `libpq-dev` from your package manager.
    This is required to build the PostgreSQL backend.
* Run `make sandbox` followed by `make deps`.
    This will set up a local sandbox for your Selda hacking, and install all
    Haskell necessary dependencies.
* Familiarise yourself with the various targets in the makefile.
    The dependencies between Selda, the backends and the tests are slightly
    complex, so straight-up cabal is too quirky for day to day hacking.


Setting up a VM for PostgreSQL testing
--------------------------------------

While the SQLite backend is completely self-contained, the PostgreSQL backend
needs an appropriate server for testing. Setting this up in a virtual machine
is definitely less intrusive than setting up a server on your development
machine. To set up a VM for the PostgreSQL backend tests:

* Install your favourite hypervisor, such as VMWare player or VirtualBox.
* Download a pre-built PostgreSQL VM from
    [Bitnami](https://bitnami.com/stack/postgresql/virtual-machine).
* Import the OVA file into your hypervisor.
* Change the network settings of your newly imported VM to NAT, and make sure
    that port 5432 is forwarded. Note that this will conflict with any PostgreSQL
    server running on your machine while the VM is running.
* Boot your VM and note the password displayed on the login screen.
* Create the file `selda-tests/PGConnectInfo.hs` with the following content:
    ```haskell
    {-# LANGUAGE OverloadedStrings #-}
    module PGConnectInfo where
    import Database.Selda.PostgreSQL
    
    pgConnectInfo = "test" `on` "localhost" `auth` ("postgres", "$PASSWORD")
    ```
    Where `$PASSWORD` is the password from the VM's login screen.
* Log in to the VM and disable the built-in firewall by running
    `sudo systemctl disable ufw ; sudo systemctl stop ufw`.
* From your host machine, create the test database:
    ```
    $ psql -h 127.0.0.1 -U postgres -W
    [password from login screen]
    # CREATE TABLE test;
    # \q
    ```
* Run `make pgtest` to check that everything works.


TODOs
-----

Features that would be nice to have but are not yet implemented.

* Monadic if/else.
* Streaming
* MySQL/MariaDB backend.
* MSSQL backend.
