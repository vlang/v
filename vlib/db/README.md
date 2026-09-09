## Description

`db` is a namespace that contains several useful modules
for operating with databases (SQLite, MySQL, MSQL, etc.)

## Common Driver Interface

The top-level `db` module exposes a small `Driver` interface for code that only needs
common SQL operations:

```v ignore
import db

mut conn := db.open(db.DriverConfig{
	kind: .sqlite
	path: ':memory:'
})!
defer { conn.close() or {} }

rows := conn.exec('select 1 as n')!
println(rows[0].val(0))
```

SQLite support is available by default. The PostgreSQL, MySQL, and MSSQL adapters are
compiled in only when their C client libraries are enabled:

- PostgreSQL: `-d db_pg`
- MySQL: `-d db_mysql`
- MSSQL/ODBC: `-d db_mssql`

For backend-specific features, continue using `db.pg`, `db.mysql`, `db.sqlite`, or
`db.mssql` directly.

## Cross-driver consistency helpers

`db.pg` and `db.mysql` accept both `user` and `username` in their `Config` structs.

`db.pg`, `db.mysql`, and `db.sqlite` rows expose `row.val(index)` and `row.values()`
for direct string access. In `db.pg`, SQL `NULL` remains available through
`row.val_opt(index)`.

`db.pg`, `db.mysql`, and `db.sqlite` also expose `exec_param2(...)` as a convenience
wrapper around their parameterized query helpers.
