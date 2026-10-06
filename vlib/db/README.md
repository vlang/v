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

## Shared connection pool

`open_pooled` creates a lazy pool over any built-in `Driver`. Each operation checks out
one connection and releases it after the operation, including after a query error.
SQLite's `:memory:` databases belong to individual connections; use a single connection
or a file-backed database when statements must share their data.

```v ignore
import db

mut database := db.open_pooled(db.DriverConfig{
	kind: .sqlite
	path: ':memory:'
}, max_open_conns: 1, max_idle_conns: 1)
defer { database.close() or {} }

database.exec('create table users (id integer primary key, name text)')!
database.exec_param_many('insert into users (name) values (?)', ['alice'])!
println(database.exec_one('select name from users')!.val(0))
```

`PoolConfig` sets `max_open_conns` (zero means unlimited), `max_idle_conns` (default two),
and `conn_max_lifetime` (zero disables expiration). The limits can also be changed on
`DB` with `set_max_open_conns`, `set_max_idle_conns`, and `set_conn_max_lifetime`.
`stats()` reports open, idle, in-use, and waiting connections. Negative count limits
are normalized to zero.

Implement `DriverFactory.connect() !&Driver` to use a third-party driver with `new_pool`
or `new_db`. The factory must support concurrent calls and return an independent physical
connection each time. `Pool.acquire()` and `DB.acquire()` return fresh `Conn` handles;
`Conn.close()` returns the physical connection. A released handle remains invalid even
when another caller acquires the same physical connection. The pool calls `Driver.reset()`
before reusing a released connection and discards connections whose reset fails. Reused
connections are validated during acquisition, including direct handoffs to waiting callers;
invalid and expired connections are discarded.

Reset behavior depends on the driver. Some built-in drivers implement `reset()` as a no-op;
the pool does not guarantee a rollback or restore session settings. Finish manual transactions
and perform any required session cleanup on the checked-out `Conn` before releasing it.

Closing a pool wakes waiting callers and closes idle connections. Checked-out connections
remain usable until released, when they are closed. Acquisition errors from the factory
are returned to the caller; construction itself does not connect. `db.open()` and existing
backend-specific pools retain their current APIs.

Use a checked-out `Conn` to pin several statements to one session. The shared pool does
not yet provide an ORM adapter or a pooled prepared-statement manager.

## Shared transactions

`DB.begin()` returns a `Tx` that owns one connection until `commit()` or `rollback()`.
Queries through the transaction always use that connection, while other database operations
acquire their own connections and may wait for pool capacity. Always finish a transaction;
defer a rollback so an early return or a query error does not keep a connection checked out.

```v ignore
mut tx := database.begin()!
defer { tx.rollback() or {} }

tx.exec_param_many('insert into users (name) values (?)', ['bob'])!
tx.savepoint('before_update')!
tx.exec_param_many('update users set name = ? where name = ?', ['bobby', 'bob'])!
tx.rollback_to('before_update')!
tx.release_savepoint('before_update')!
tx.commit()!
```

`Tx` exposes `exec`, `exec_one`, and `exec_param_many`, plus `savepoint`, `rollback_to`,
and `release_savepoint`. Savepoint names must start with an ASCII letter or underscore and
contain only ASCII letters, digits, or underscores. Built-in drivers quote these identifiers,
so SQL keywords can be used as names. MSSQL limits names to 32 characters and does not support
`release_savepoint`; it returns an error without finishing the transaction.

Transaction operations are serialized. A commit or rollback attempt finishes the transaction
even if it fails; subsequent calls, including through a copied handle, return an error.
A failed begin, commit, or rollback closes the physical connection to prevent reuse of an
uncertain transaction state. Successful completion returns it through the pool's reset hook.
Closing the database lets an existing transaction finish and closes its connection on release.

The default transaction commands use `BEGIN`, `COMMIT`, `ROLLBACK`, and ANSI SQL savepoints,
with the database's default isolation level. A driver can implement the optional
`TransactionDriver.transaction(command TransactionCommand, name string) !` interface to
use its native transaction API or SQL dialect. This keeps the existing `Driver` interface
unchanged. The hook receives a validated, unquoted savepoint name, or an empty name for other
commands; it must report database failures accurately. Built-in SQLite checks transaction
result codes, MySQL uses backtick identifiers, and MSSQL uses its own transaction syntax.

## Cross-driver consistency helpers

`db.pg` and `db.mysql` accept both `user` and `username` in their `Config` structs.

`db.pg`, `db.mysql`, and `db.sqlite` rows expose `row.val(index)` and `row.values()`
for direct string access. In `db.pg`, SQL `NULL` remains available through
`row.val_opt(index)`.

`db.pg`, `db.mysql`, and `db.sqlite` also expose `exec_param2(...)` as a convenience
wrapper around their parameterized query helpers.
