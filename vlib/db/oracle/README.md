# db.oracle

Oracle Database driver for V, built on the Oracle Call Interface (OCI) and
implementing `orm.Connection` / `orm.TransactionalConnection`.

## Status

Experimental. The module compiles and links against Oracle Instant Client
23ai (23.26) on Windows x64. Connection and query execution have **not** been
verified against a live database; the live test is gated behind
`started_oracle`.

## Installing the client

Download the **Basic** and **SDK** packages for Windows x64 from
<https://www.oracle.com/database/technologies/instant-client/winx64-64-downloads.html>
(no login required), unzip both into the same directory, and point
`ORACLE_CLIENT_DIR` at the resulting `instantclient_<version>` folder:

```powershell
$env:ORACLE_CLIENT_DIR = 'C:\oracle\instantclient_23_26'
$env:PATH = "$env:ORACLE_CLIENT_DIR;$env:PATH"
```

The first flag file links `sdk/lib/msvc/oci.lib`; the matching `oci.dll` and
`oraociei.dll` must be reachable through `PATH` at runtime. The Basic Light
package is not sufficient — it omits some globalisation files.

V never reads `oci.h`. All types and function prototypes are declared manually
in `_cdefs.c.v`, as required by the V C-interop rules. `oci_decls.h` only gives
the C compiler the same prototypes when this module is imported. The driver
uses the full OCI handle chain with `OCISessionBegin` instead of the simpler
`OCILogon2`, so transactions can be controlled explicitly.

`v test` needs the client directory on `PATH` as well, because the generated
test executables load `OCI.dll` at process start.

## Usage

```v
import db.oracle

struct User {
	id   int @[primary; sql: serial]
	name string
}

fn main() {
	mut conn := oracle.open(
		host:     'localhost'
		port:     1521
		service:  'FREE'
		user:     'system'
		password: 'oracle'
	)!
	defer {
		conn.close()
	}

	sql conn {
		create table User
	}!

	user := User{ name: 'vlang' }
	sql conn {
		insert user into User
	}!

	users := sql conn {
		select from User
	}!
	println(users.len)
}
```

Raw SQL uses positional binds (`:1`, `:2`, ...):

```v
import db.oracle

fn main() {
	mut conn := oracle.open(dsn: '//localhost:1521/FREE', user: 'system', password: 'oracle')!
	defer {
		conn.close()
	}
	res := conn.exec('SELECT "name" FROM "users" WHERE "id" = :1', [
		oracle.Param{ value: '1' },
	])!
	println(res.rows.len)
}
```

Transactions are available through both the explicit methods
(`begin`, `commit`, `rollback`, `savepoint`, `rollback_to`,
`release_savepoint`) and the ORM transaction helpers.

## SQL generation

The driver reuses the `.default` dialect in `vlib/orm` and rewrites only the
`LIMIT/OFFSET` tail into Oracle's standard form:

```sql
OFFSET :2 ROWS FETCH FIRST :1 ROWS ONLY
```

Identifiers are quoted with `"`. Types map to `NUMBER` for integers,
`NUMBER` for floats, and `VARCHAR2(4000)` for strings. A `serial` field gets a
named sequence (`<table>_<field>_SEQ`) used as the column default, and
`last_id()` reads that sequence's `CURRVAL`.

## Limitations

- No connection pooling; each `Connection` owns one OCI session.
- All bind and result values cross the OCI boundary as strings and are
  converted by Oracle. Very large `NUMBER` values may lose precision.
- `CREATE TABLE IF NOT EXISTS` is stripped; `ORA-00955` (existing object) is
  tolerated by `create()`.
- Time values are stored as Unix epoch numbers, not Oracle `TIMESTAMP`.
- `VARCHAR2(4000)` is the default string width; widen the column manually if
  `MAX_STRING_SIZE = EXTENDED` is enabled.
- Output column names are not yet exposed through `Result.names` in the ORM
  `select` path (they are populated in `execute()`).
