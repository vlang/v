# db.h2

An H2 Database driver for V's ORM. It exposes `H2Connection`, which implements
`orm.Connection` and `orm.TransactionalConnection`, so `sql db { ... }` works
against an H2 database.

The driver talks to H2 through JDBC using the JNI Invocation API. The JVM is
created once per process; `jvm.dll` is loaded at runtime from `JAVA_HOME`, and
the H2 jar is placed on the JVM classpath from `H2_JAR`. All C/JNI code lives
in `h2_shim.c`, `h2_shim.h` and `shim.c.v`; the V compiler never parses
`jni.h`.

## Requirements

- A JDK (17 or newer) whose `JAVA_HOME` contains:
  - `bin/server/jvm.dll` (Windows) or `lib/server/libjvm.so` (Linux)
  - `include/jni.h`
  - `include/win32/jni_md.h` (Windows)
- The H2 jar (2.x), for example `h2-2.3.232.jar`.

## Setup

Set the environment variables before compiling a program that imports
`db.h2` (V uses `JAVA_HOME` at compile time to find the JNI headers):

```sh
export JAVA_HOME=/path/to/jdk
export H2_JAR=/path/to/h2.jar
```

On Windows (PowerShell):

```powershell
$env:JAVA_HOME = 'C:\path\to\jdk'
$env:H2_JAR = 'C:\path\to\h2.jar'
```

`H2_JAR` is read both at compile time (to place the jar on the classpath) and
at run time. It must point to a real jar file.

## Usage

```v
import db.h2

struct User {
	id   int @[primary; sql: serial]
	name string
	age  int
}

mut conn := h2.connect(':memory:') or { panic(err) }
defer {
	conn.close()
}

// Raw JDBC-style queries with `?` placeholders. NULL values are tracked in
// Result.nulls.
rows := conn.exec('SELECT "name" FROM "People" WHERE "age" > ?', ['20'])!
println(rows.vals)

// ORM
sql conn {
	create table User
}!
ada := User{ name: 'Ada', age: 36 }
sql conn {
	insert ada into User
}!
users := sql conn {
	select from User where age > 20 order by age limit 10 offset 0
}!
println(users)
```

`connect(':memory:')` (or `connect('')`) opens a private in-memory database
with `DB_CLOSE_DELAY=-1`. Any other path is opened as `jdbc:h2:<path>`, i.e. a
file-backed database whose name is the supplied path.

## Implementation notes

- Parameters are bound with `PreparedStatement.setString`, except for plain
  integers (used by `setInt`) and SQL NULL (`setNull`), so H2 accepts
  `LIMIT ?` / `OFFSET ?` placeholders.
- `last_id()` uses JDBC generated keys (`RETURN_GENERATED_KEYS`) because H2 2.x
  no longer exposes `IDENTITY()` or `LAST_INSERT_ID()`.
- Columns are created with double-quoted identifiers; H2 treats quoted names
  as case-sensitive, so create and query with consistent casing.
- The JVM is not destroyed on `close()`; it stays alive until process exit so
  that several connections can be opened in one process.
- Text is exchanged using JNI modified UTF-8; supplementary Unicode characters
  beyond the BMP are not supported yet.
