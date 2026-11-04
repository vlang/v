## Description

`sqlite` is a thin wrapper for [the SQLite library](https://sqlite.org/), which in turn is
"a C-language library that implements a small, fast, self-contained,
high-reliability, full-featured, SQL database engine."

# Install SQLite Dependency

On **any platform** (Windows, Linux, macOS), you can run:

`v vlib/db/sqlite/install_thirdparty_sqlite.vsh`

This downloads the SQLite amalgamation source and places it in
`v/thirdparty/sqlite`. V will then compile it automatically
during your build.

The installer pins one amalgamation version (currently 3.53.4) instead of
taking the newest. SQLite 3.54.0 uses the `SRWLOCK` type in its Windows mutex
code, which tcc's `windows.h` does not declare, so `v -cc tcc` on Windows
cannot build 3.54.0. The pin moves only when someone updates it by hand, after
checking that tcc builds the new amalgamation. SQLite fixes released after
3.53.4 are not picked up automatically.

If you need to install SQLite manually, use the SQLite
**amalgamation** package. `v/thirdparty/sqlite` must contain
`sqlite3.c` and `sqlite3.h` directly.

Do not use the autoconf/source package here, and do not rename
`sqlite3.c` to `sqlite3.cpp`.

On **macOS**, V can also fall back to the system `libsqlite3`
when `pkg-config` is not available. It includes `sqlite3.h` from
the macOS SDK and links the system library, so an extra SQLite install is not needed.
Install Xcode Command Line Tools if the SDK headers are missing.

On **Linux**, you can also install the system development package
instead:

- Debian/Ubuntu: `sudo apt install -y libsqlite3-dev`
- Fedora/RHEL: `sudo dnf -y install sqlite-devel`
- Arch: `sudo pacman -S sqlite`

# Convenience Methods

The `DB` struct provides several helper methods for common
introspection queries:

```v
import db.sqlite

db := sqlite.connect('mydb.db') or { panic(err) }

// List all user tables
tables := db.tables()!

// Get column names for a table
cols := db.columns('users')!

// Get CREATE statements (single table or all objects)
s := db.schema('users')!

// Database file size in bytes
size := db.db_size()!
```

# Interactive CLI

V includes a built-in SQLite CLI as a replacement for `sqlite3`:

```sh
v sqlite mydb.db
```

Features include a full readline REPL with history and tab
completion, 9 output modes (`table`, `box`, `markdown`, `csv`,
`json`, `line`, `html`, `insert`, `quote`), `.dump`,
`.import`/`.export`, `.backup`, session control, and schema tools.
Run `.help` inside the REPL for the full command list.

# Performance Tips

When performing a large amount of database calls (i.e. INSERTS), significant
performance increase can be obtained by controlling the synchronization and journal modes.

For instance:
```v
import db.sqlite

db := sqlite.connect('foo.db') or { panic(err) }
db.synchronization_mode(sqlite.SyncMode.off)!
db.journal_mode(sqlite.JournalMode.memory)!
```

# Concurrent Access

When several connections use the same database file, write-ahead logging lets readers work
while a transaction writes; there is still only one writer at a time, so applications must
handle `SQLITE_BUSY`. An `.immediate` transaction takes the write lock when it starts, waiting
for it up to the busy timeout. A `.deferred` transaction, the default, takes it at its first
write; if it has already read by then, SQLite can return `SQLITE_BUSY` without waiting.

```v
import db.sqlite

mut db := sqlite.connect('app.db') or { panic(err) }
defer {
	db.close() or {}
}
db.busy_timeout(5000)
db.journal_mode(.wal)!
if db.current_journal_mode()! != .wal {
	eprintln('write-ahead logging is not available for this database')
}
db.begin(transaction_level: .immediate)!
db.exec('CREATE TABLE IF NOT EXISTS t (x INTEGER)')!
db.commit()!
```

`journal_mode` can leave the current mode in place without an error, e.g. an in-memory database
stays in `.memory` mode: `current_journal_mode` returns the mode that applies.
