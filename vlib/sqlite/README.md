## Description

`sqlite` is a thin wrapper for [the SQLite library](https://sqlite.org/), which in turn is
"a C-language library that implements a small, fast, self-contained,
high-reliability, full-featured, SQL database engine."

# Install SQLite Dependency

Before you can use this module, you must first have the SQLite development
library installed on your system.

**Fedora 31**:

`sudo dnf -y install sqlite-devel`


**Ubuntu 20.04**:

`sudo apt install -y libsqlite3-dev`


**Windows**:
- Download the source zip from [SQLite Downloads](https://sqlite.org/download.html)
- Create a new `sqlite` subfolder inside `v/thirdparty`
- Extract the zip into that folder

# Performance Tips

When performing a large amount of database calls (i.e. INSERTS), significant
performance increase can be obtained by controlling the synchronization and journal modes.

For instance:
```v
import sqlite

db := sqlite.connect('foo.db') or { panic(err) }
db.synchronization_mode(sqlite.SyncMode.off)
db.journal_mode(sqlite.JournalMode.memory)
```
