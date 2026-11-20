# db.duckdb

A DuckDB driver for V. It implements `orm.Connection` and
`orm.TransactionalConnection`, so `db.duckdb.DB` can be used directly with V's
`sql db { ... }` syntax.

The module binds the DuckDB C API (v2.0.0) directly. No header is parsed by V;
the C declarations live in `duckdb.c.v`, and link flags live in `_cdefs.c.v`.

## Installation

The DuckDB shared library and its header are required.

- **Windows**: download a `duckdb-shared-libs-windows-amd64` release containing
  `duckdb.h`, `duckdb.lib` and `duckdb.dll`. Set `LIBDUCKDB_DIR` to that
  directory when compiling, and add it to `PATH` when running:

  ```powershell
  $env:LIBDUCKDB_DIR = 'C:\path\to\libduckdb'
  $env:PATH += ";$env:LIBDUCKDB_DIR"
  v run app.v
  ```

- **Linux/macOS**: `pkg-config duckdb` is used when available. Otherwise set
  `LIBDUCKDB_DIR` to a prefix containing `include/duckdb.h` and `lib/` (or use
  the standard `/usr/local`, Homebrew and MacPorts locations). Ensure the
  shared library is discoverable at runtime (`LD_LIBRARY_PATH` /
  `DYLD_LIBRARY_PATH`).

## Basic usage

```v
import db.duckdb

mut db := duckdb.connect(':memory:')!
defer {
	db.close() or { panic(err) }
}

db.exec('CREATE TABLE people (name TEXT, age BIGINT)')!
rows := db.exec_param('SELECT name, age FROM people WHERE age > ?1 ORDER BY age', ['20'])!
for row in rows {
	println('${row.get_string('name')}: ${row.get_string('age')}')
}
```

`Row` stores values as strings, keeps column names in `names`, and tracks SQL
NULL precisely in `is_null`. Use `row.is_null(i)` to distinguish NULL from the
empty string. `exec`, `exec_one`, `exec_map`, `exec_param`, `q_int`, `q_string`
and `rows_changed` are available for raw SQL.

Prepared statements are exposed through `Stmt` (use `db.prepare`/`bind_*`/
`exec`/`finalize`); the ORM uses them internally for every bound value.

## ORM usage

```v
import db.duckdb

struct Person {
	id   int @[primary; sql: serial]
	name string
	age  int
}

mut db := duckdb.connect(':memory:')!
defer { db.close() or { panic(err) } }

sql db {
    create table Person
}!

p := Person{ name: 'Alice', age: 30 }
id := sql db {
    insert p into Person
}!
assert id == 1

people := sql db {
    select from Person where age > 20 limit 10
}!
```

The driver reuses the `orm.SQLDialect.sqlite` spelling (backtick-quoted
identifiers, `?1` positional parameters and `LIMIT/OFFSET`). DuckDB accepts all
of these; no change to `vlib/orm` is needed.

### Type mapping

| V type (ORM)                                   | DuckDB type |
| ---------------------------------------------- | ----------- |
| integers, bool, serial, `time.Time`, enums     | `BIGINT`    |
| `f32`, `f64`                                   | `DOUBLE`    |
| `string`                                       | `TEXT`      |

`time.Time` is stored as Unix seconds. Enums are stored as their integer index.

## Transactions and savepoints

`BEGIN TRANSACTION`, `COMMIT` and `ROLLBACK` are supported and are available
both through `orm.begin(...)` and the convenience methods `begin`/`commit`/
`rollback`.

DuckDB 2.0 does **not** implement `SAVEPOINT`, `ROLLBACK TO` or `RELEASE`.
`orm_savepoint`, `orm_rollback_to` and `orm_release_savepoint` therefore return
explicit errors instead of silently doing nothing.

## `last_id` limitation

DuckDB has no `last_insert_rowid()`. When `create` sees a serial primary key,
this driver:

1. creates a sequence named `seq_<table_name>` (`IF NOT EXISTS`), and
2. injects `DEFAULT nextval('seq_<table_name>')` into that column's DDL.

`last_id()` then returns `currval()` of the sequence used by the most recent
insert on the connection. Consequences:

- it only works for tables created by this driver with a serial primary key;
- for other tables it returns `0`;
- the sequence is per table, so concurrent inserts from other connections to
  the same table are not reflected in this connection's `last_id()`.

If you need the exact generated key for an arbitrary insert, add a `RETURNING`
clause to your own SQL instead.

## Known DuckDB 2.0 C API quirk

`duckdb_value_string()` / `duckdb_value_is_null()` can return NULL or crash for
result sets that contain **constant-folded NULL expressions** directly in the
`SELECT` list, e.g. `SELECT NULL AS z, 3 AS y`. Values read from ordinary table
columns are unaffected (including NULL table columns). Avoid such constant NULL
expressions, or use `SELECT NULL::VARCHAR` / a CTE backed by a table.

## Tests

The test files are gated with `// vtest build: present_duckdb?`. To run them
locally with the library available:

```powershell
v -d present_duckdb test vlib/db/duckdb
```