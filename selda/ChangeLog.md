# Revision history for selda

## 0.1.4.1 -- 2017-05-04

* Fix cache consistency bug in the presence of multiple databases.


## 0.1.4.0 -- 2017-05-04

* Add uniqueness constraints and foreign keys.


## 0.1.3.3 -- 2017-05-04

* Fix cache invalidation race when using transactions.


## 0.1.3.2 -- 2017-05-01

* Only throw well-documented, Selda-specific exceptions.


## 0.1.3.1 -- 2017-05-01

* More hackage-friendly README.


## 0.1.3.0 -- 2017-04-30

* Add selectors for non-generic tables.
* Allow default insertions on all columns.
* More sensible API for LIMIT.
* Fix broken SQL being generated for pathological corner cases.
* Documentation fixes.


## 0.1.2.0 -- 2017-04-20

* Replace `¤` with `:*:` in table definitions.


## 0.1.1.1 -- 2017-04-20

* Generic tables, queries and mutation.
* Select from inline tables.
* Tutorial updates.
* Minor bugfixes.


## 0.1.0.0 -- 2017-04-14

* Initial release.
