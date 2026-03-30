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
